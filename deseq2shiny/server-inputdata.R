observe({
    # Hide the loading message when the rest of the server function has executed
    # hide(id = "loading-content", anim = TRUE, animType = "fade")
    #
    # removeClass("app-content", "hidden")


    # shinyjs::hide(selector = "a[data-value=\"conditionsTab1\"]")
    # print("server-inputdata")

    shinyjs::hide(selector = "a[data-value=\"deseqTab\"]")
    shinyjs::hide(selector = "a[data-value=\"svaseqTab\"]")
    shinyjs::hide(selector = "a[data-value=\"conditionsTab\"]")

    shinyjs::hide(selector = "a[data-value=\"rlogTab\"]")
    shinyjs::hide(selector = "a[data-value=\"vstTab\"]")

    shinyjs::hide(selector = "a[data-value=\"resultsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"venndiagramTab\"]")
    shinyjs::hide(selector = "a[data-value=\"volcanoplotTab\"]")
    shinyjs::hide(selector = "a[data-value=\"boxplotTab\"]")
    shinyjs::hide(selector = "a[data-value=\"heatmapTab\"]")

    shinyjs::disable("computeVST")
    # Check if example selected, or if not then ask to upload a file.

    # shiny:: validate(
    #   need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))),
    #        message = "Please select a file")
    # )
    # inFile <- input$datafile

    inputFileReactive()
})


decryptUrlParam <- function(cipher) {
    keyHex <- readr::read_file("private.txt")

    key <- hex2bin(keyHex)
    cipher <- hex2bin(cipher)

    orig <- simple_decrypt(cipher, key)

    unserialize(orig)
}



inputFileReactive <- reactive({
    # print('inputFileReactive')
    query <- parseQueryString(session$clientData$url_search)
    # print(query)
    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need(identical(input$data_file_type, "examplecounts") | identical(input$data_file_type, "examplecountsfactors") | (!is.null(input$datafile)) | (!is.null(query[["countsdata"]])),
            message = "Please select a file"
        )
    )

    # print('valida')

    if (!is.null(query[["countsdata"]])) {
        inFile <- decryptUrlParam(query[["countsdata"]])
        # print('ask inputdata')
        shinyjs::show(selector = "a[data-value=\"inputdata\"]")
        shinyjs::disable("datafile")
        # js$collapse("uploadbox")
    } else if (input$data_file_type == "countsFile") {
        inFile <- input$datafile
        if (is.null(inFile)) {
            return(NULL)
        }

        inFile <- inFile$datapath
    } else if (input$data_file_type == "examplecounts") {
        inFile <- "www/exampleData/wang_count_table.csv"
        updateCheckboxInput(session, "no_replicates", value = "true")
    } else if (input$data_file_type == "examplecountsfactors") {
        inFile <- "www/exampleData/chenCounts.csv"
        if (input$no_replicates) {
            updateCheckboxInput(session, "no_replicates", value = "false")
        }
    }

    # print('inFile')
    # print(inFile)


    # select file separator, either tab or comma
    sep <- "\t"
    if (length(inFile) > 0) {
        testSep <- read.csv(inFile[1], header = TRUE, sep = "\t")
        if (ncol(testSep) < 2) {
            sep <- ","
        }
    } else {
        return(NULL)
    }

    fileContent <- read.csv(inFile[1], header = TRUE, sep = sep)

    if(input$gene_alias == 'notincluded'){
        sampleN <- colnames(fileContent)[-1]
    } else {
        sampleN <- colnames(fileContent)[c(-1,-2)]

        chosen_column <- "gene.names"
        print(chosen_column)
        
        df <-  fileContent
       
        duplicated_names <- df[[2]][duplicated(df[[2]])]
        print(duplicated_names)
        duplicate_rows <- df[df[[2]] %in% duplicated_names, ]
        print(duplicate_rows)
        output$duplicate_contents <- renderDataTable({duplicate_rows}, options = list(scrollX = TRUE))

        genenames <-fileContent[, c(2)]
        genenames<- make.unique(genenames, sep = '--')
        fileContent[, c(2)] <- genenames

        genenames <- as.data.frame(fileContent[, c(2)])
        geneids <- as.data.frame(fileContent[, c(1)])
        
        row.names(geneids) <- fileContent[, 2]
        row.names(genenames) <- fileContent[, 1]
        genenames <- as.data.frame(genenames)
        geneids <- as.data.frame(geneids)
        #print(genenames)
        # output$d <-TRUE
        

        myValues$geneids <- geneids
        myValues$genenames <- genenames
        print('length(duplicated_names)')
        print(duplicated_names)
        print(length(duplicated_names))
        if(length(duplicated_names)>0) {
        output$conditional_output <- renderUI({
        div(
             p(style = "color:red;", "Gene name duplicates have been identified. We have renamed all duplicate gene names by appending \"--{replicate number}\" to the gene name. Below is a table listing the duplicate genes. You have the option to either remove the duplicate entries or proceed with the renamed gene names"),
            h2("Duplicate Gene Counts Table"), hr(),
            withSpinner(dataTableOutput("duplicate_contents"))

        )   
        })
        }
       

    }

    
    dataCounts <- fileContent[, sampleN]
    dataCounts <- data.frame(sapply(dataCounts, as.integer))



    row.names(dataCounts) <- fileContent[, 1]
    myValues$fileContent <- fileContent
    myValues$dataCounts <- as.matrix(dataCounts)

    if (is.null(query[["countsdata"]])) {
        js$collapse("uploadbox")
    }
 
    # print('dataCounts.....')
    # print(dataCounts)

   

    return(dataCounts)
})

output$contents <- renderDataTable(
    {
        tmp <- inputFileReactive()
        

        df <- myValues$fileContent
        row.names(df) <- df[, 1]
        
        # test = myValues$dataCounts
        if (!is.null(tmp)) df[row.names(myValues$dataCounts),][,-1]
        # if (!is.null(tmp)) myValues$dataCounts
    },
    options = list(scrollX = TRUE)
)


observeEvent(input$prefilterCounts, ignoreInit = TRUE, {
    dataCounts <- inputFileReactive()
    dataCounts <- dataCounts[rowSums(dataCounts) >= input$minRowCount, ]
    myValues$dataCounts <- dataCounts
    print("prefilterCounts")
})

myValues <- reactiveValues()
myValues$selected_genes <- 0


output$deseqMenu <- renderMenu({
    if (!is.null(csvDataReactive())) {
        menuItem("2. Run DESeq2",
            tabName = "preprocTab", icon = icon("th"), startExpanded = T,
            menuSubItem("Design Conditions", tabName = "conditionsTab"),
            menuSubItem("Hidden Batch Effect", tabName = "svaseqTab"),
            menuSubItem("Run DESeq2", tabName = "deseqTab")
        )
    }
})


observe({
    csvDataReactive()
})

csvDataReactive <- eventReactive(input$submit, {
    fileContent <- inputFileReactive()

    shinyjs::show(selector = "a[data-value=\"conditionsTab\"]")
    updateTabItems(session, "tabs", "conditionsTab")
    shinyjs::runjs("window.scrollTo(0, 0)")

    sampleN <- colnames(fileContent)

    if (identical(input$data_file_type, "countsFile")) {
        if (input$no_replicates) {
            sampleConditions <- sampleN

            samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
        } else {
            sampleConditions <- strsplit(sampleN, "_")

            sampleConditions <- unlist(lapply(sampleConditions, function(x) {
                x[1]
            }))

            samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
        }
    } else if (identical(input$data_file_type, "examplecounts")) {
        updateCheckboxInput(session, "no_replicates", value = T)


        sampleConditions <- sampleN
        # samples <- data.frame(row.names = sampleN, condition = sampleConditions)
        samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
    } else {
        samples <- read.csv("www/exampleData/chenMeta.csv", header = TRUE, sep = ",", row.names = 1)
    }


    myValues$DF <- samples
    myValues$DF[] <- lapply(myValues$DF, as.factor)

    updateDesignFormula()

    return(list("countsData" = fileContent))
})

output$fileUploaded <- reactive({
    return(!is.null(inputFileReactive()))
})
outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

output$noreplicates <- reactive({
    return(input$no_replicates)
})
outputOptions(output, "noreplicates", suspendWhenHidden = FALSE)


observe({
    if (input$data_file_type %in% c("examplecountsfactors", "countsFile")) {
        updateCheckboxInput(session, "no_replicates", value = F)
        test <- inputFileReactive()
    }
})
