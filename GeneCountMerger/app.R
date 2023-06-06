# Installl missing packages
list.of.packages <- c("shiny", "shinyBS", "parallel", "shinyjs", "sodium", "uuid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages, repos = "https://cloud.r-project.org/", dependencies = T)
}


library(shiny)
library(shinyBS)
library(parallel)
library(shinyjs)
library(sodium)
library(uuid)
library(readr)



ui <- tagList(
    tags$head(
        tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
        tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
        tags$style(".BoxArea2 { padding:19px; margin: 5px; border: 3px solid; border-color:#c7dbe6; border-radius:10px;}"),
        tags$style(".BoxArea3 { padding:19px; margin: 5px; border: 3px solid; border-color:#ced2d6; border-radius:10px;}"),
        includeCSS("www/custom.css")
    ),
    navbarPage(
        theme = "https://bootswatch.com/3/cerulean/bootstrap.min.css",
        inverse = T,
        title = "Gene Count Merger (Pre-processing)",
        tabPanel("Home",
            icon = icon("home"),
            useShinyjs(), # Set up shinyjs
            fluidRow(
                column(
                    4,
                    bsCollapse(
                        id = "input_collapse_panel", open = "data_panel", multiple = FALSE,
                        bsCollapsePanel(
                            title = "Upload Files", value = "data_panel", style = "primary",
                            p("1. Select multiple files containing counts to upload (eg. output of htseq counts)"),
                            tags$img(src = "inputFiles.png", width = "100px", height = "100px"),
                            p(strong("Note: "), "File names will be used as sample (column) names in output table"),
                            radioButtons("mergeType", "",
                                c(
                                    "Merge individual sample counts" = "single",
                                    "Merge 2 or more matrices" = "multiple"
                                ),
                                selected = "single"
                            ),
                            fileInput("datafile", "",
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                                ), multiple = TRUE
                            )
                        ),
                        bsCollapsePanel(
                            title = "Options", value = "analysis_panel",
                            checkboxInput("addOne", "Add +1 to counts (Pseudocounts)", FALSE),
                            checkboxInput("addGeneNames", "Retrieve gene names from ensembl ids", FALSE),
                            conditionalPanel(
                                "input.addGeneNames",
                                wellPanel(
                                    selectInput("refGenome", "Select genome/version:",
                                        c(
                                            "Homo_sapiens.GRCh38.81",
                                            "Homo_sapiens.GRCh38.84",
                                            "Mus_musculus.GRCm38.82",
                                            "Danio_rerio.GRCz10.84",
                                            "Drosophila_melanogaster.BDGP6.81",
                                            "c_elegans.PRJNA13758.WS283",
                                            "Other (not listed)"
                                        ),
                                        selected = "Homo_sapiens.GRCh38.81"
                                    ),
                                    conditionalPanel(
                                        "input.refGenome=='Other (not listed)'",
                                        a(href = "https://github.com/nyuad-corebio/Nasqar2/tree/main//GeneidToNameFromGtf", "Click here if you have a .gtf file for your genome", target = "_blank"),
                                        fileInput("gtfMappingFile", "Upload gene/id lookup table (.csv)",
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            ), multiple = F
                                        )
                                    ),
                                    radioButtons("geneNameColumn", "",
                                        c(
                                            "Add gene.names column after gene ids" = "add",
                                            "Replace gene ids column by gene names" = "replace"
                                        ),
                                        selected = "replace"
                                    )
                                )
                            ),
                            conditionalPanel(
                                "output.filesUploaded",
                                actionButton("upload_data", "Merge Files", class = "btn-danger")
                            ),
                            conditionalPanel(
                                "output.filesMerged",
                                hr(),
                                wellPanel(
                                    style = "background-color: #ffffff;",
                                    uiOutput("tab")
                                )
                            )
                        )
                    ) # bscollapse
                ), # column
                column(
                    8,
                    mainPanel(
                        width = 12,
                        tabsetPanel(
                            id = "tabs",
                            tabPanel(
                                "User Guide",
                                hr(),
                                h4(strong("1) Introduction:")),
                                wellPanel(
                                    p("This is a simple preprocessing tool to merge individual gene count files (Eg. output count files from htseq)"),
                                    p(strong("NOTE:"), "first column must contain the genes. If the gene columns do not match in all files, this tool will not work"),
                                    hr(),
                                    h5(strong("Features")),
                                    tags$ul(
                                        tags$li("Merge individual sample count files. See ", strong("Sample Input Files"), " below for more details"),
                                        tags$li("Or merge", strong(" multiple matrices")),
                                        tags$li(
                                            strong("Convert ensembl gene IDs to gene names"),
                                            tags$ul(
                                                tags$li("Option to choose from available genome/versions"),
                                                tags$li("If genome/version is not available in the options and you have a ", a(target = "_blank", href = "https://asia.ensembl.org/info/website/upload/gff.html", ".gtf"), " file for your genome", a(href = "https://github.com/nyuad-corebio/Nasqar2/tree/main//GeneidToNameFromGtf", "follow these instructions."))
                                            )
                                        ),
                                        tags$li("Option to add ", strong("pseudocounts (+1)")),
                                        tags$li(strong("Download"), " merged counts file in .csv format"),
                                        tags$li(
                                            strong("Transcriptome Analysis (Optional)"), " after merging your counts:",
                                            tags$ul(
                                                tags$li("Use our ", strong("Seurat Wizard"), " to carry out single-cell RNA analysis"),
                                                tags$li("Use ", strong("DESeq2Shiny"), " or ", strong("START"), " apps to carry out bulk RNA analysis"),
                                                tags$li("If there are ", strong("NO replicates"), ", use DESeq2Shiny app for exploratory analysis")
                                            )
                                        )
                                    )
                                ),
                                hr(),
                                # wellPanel(
                                h4(strong("2) Sample Input Files:")),
                                tags$div(
                                    class = "BoxArea2",
                                    fluidRow(
                                        column(
                                            12,
                                            p(strong("Select multiple files to upload, E.g. Input files:")),
                                            column(
                                                3,
                                                p(strong(tags$em("File 1 of 8: ")), "CT6_1.txt"),
                                                tags$img(src = "inputFiles.png", width = "100%")
                                            ),
                                            column(
                                                3,
                                                p(strong(tags$em("File 2 of 8: ")), "CT6_2.txt"),
                                                tags$img(src = "inputFiles.png", width = "100%")
                                            ),
                                            column(
                                                6,
                                                p("etc ...")
                                            )
                                        ),
                                        div(style = "clear:both;")
                                    ),
                                    fluidRow(
                                        column(
                                            12,
                                            p(""),
                                            p(strong("Note: "), "File names will be used as sample (column) names in output table. You can edit the column names after merging")
                                        )
                                    )
                                ),
                                column(12, hr()),
                                h4(strong("3) Sample Output File:")),
                                div(style = "clear:both;"),
                                tags$div(
                                    class = "BoxArea2",
                                    p(strong("Output depending on options selected:")),
                                    column(
                                        12,
                                        p(strong(em("A) Without renaming/converting genes (Default)"))),
                                        tags$img(src = "output_geneids.png", width = "100%")
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    column(
                                        12,
                                        p(strong(em("B) Retrieve gene names (replace), E.g. output file"))),
                                        tags$img(src = "output_genenames.png", width = "100%")
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    column(
                                        12,
                                        p(strong(em("C) Retrieve gene names (add), E.g. output file"))),
                                        tags$img(src = "output_both.png", width = "100%")
                                    ),
                                    div(style = "clear:both;")
                                ),
                                column(12, hr()),
                                h4(strong("4) Transcriptome Analysis (Optional):")),
                                div(style = "clear:both;"),
                                tags$div(
                                    class = "BoxArea2",
                                    p(strong("Start your analysis by launching the appropriate application for your data")),
                                    p(strong("Your merged counts data will be automatically loaded")),
                                    fluidRow(
                                        column(6,
                                            offset = 3,
                                            tags$img(src = "transcAnalysis.png", width="100%")
                                        )
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    div(style = "clear:both;")
                                )
                            ),
                            tabPanel(
                                "Output",
                                h4(p(strong("Merged counts"))),
                                hr(),
                                conditionalPanel(
                                    "output.filesMerged",
                                    conditionalPanel(
                                        "!output.columnNamesValid",
                                        div(
                                            class = "alert alert-warning",
                                            p(strong("Sample names are NOT valid for use in our analysis apps")),
                                            p("Edit the column names so they are of the following format:"),
                                            tags$ul(
                                                tags$li(strong("sampleName_replicateNumber. Eg: kidneyA_1, kidneyA_2, liver_1, liver_2, etc ...")),
                                                tags$li(strong("do NOT use any special characters in sample names except underscore'_' to denote replicate numbers"))
                                            )
                                        )
                                    ),
                                    bsCollapse(
                                        id = "editCols_collapse_panel", multiple = FALSE,
                                        bsCollapsePanel(
                                            title = "Edit Column Names", value = "editCols_panel", style = "primary",
                                            uiOutput("editColumnNamesView")
                                        )
                                    ),
                                    p(
                                        downloadLink("downloadData", "Download Merged File", class = "btn btn-warning", style = "color: #fff; background-color: #9E0000; border-color: #9E0000")
                                    ),
                                    hr(),
                                    dataTableOutput("contents")
                                ),
                                conditionalPanel(
                                    "!output.filesMerged",
                                    p(
                                        # verbatimTextOutput("mergeStatus")
                                        uiOutput("mergeStatus")
                                    ),
                                    h4(p(em(
                                        # verbatimTextOutput("mergeStatus")
                                    ), style = "color:#f56a6a;"))
                                )
                            )
                        )
                    )
                )
            ) # fluidrow
        ), # tabpanel
        tabPanel(
            "Terms of Use",
            fluidRow(
                column(10,
                    offset = 1,
                    includeMarkdown("termsConditions.Rmd")
                )
            )
        ),


        ## ==================================================================================== ##
        ## FOOTER
        ## ==================================================================================== ##
        footer = p(
            hr(), p("Gene Count Merger created by ", "Core Bioinformatics Team", " of ", align = "center", width = 4),
            p(("Center for Genomics and Systems Biology, NYU Abu Dhabi"), align = "center", width = 4),
            p(("Copyright (C) 2018, code licensed under GPLv3"), align = "center", width = 4)
        )
    ) # end navbarpage
) # end taglist




options(shiny.maxRequestSize = 60 * 1024^2)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)

    observe({
        myValues$status <- "Upload file(s) first"
    })

    myValues <- reactiveValues()

    output$mergeStatus <- renderUI({
        div(
            class = "alert alert-danger",
            strong(myValues$status)
        )
    })

    observeEvent(input$viewRenameColumns, {
        toggle("editColumnNamesView", TRUE)
    })


    output$editColumnNamesView <- renderUI({
        tmp <- analyzeDataReactive()
        if (!is.null(tmp)) {
            columnNames <- colnames(tmp$data)

            outputUI <- lapply(seq(length(columnNames)), function(i) {
                output[[paste0("textboxColumns", i)]] <- renderUI({
                    if (i == 1) {
                        column(
                            4,
                            shinyjs::disabled(textInput(paste0("textboxColumns", i), paste("Column", i), columnNames[i]))
                        )
                    } else {
                        column(
                            4,
                            # tags$label( paste("Column", i)),
                            # div(class = "input-group form-group",
                            #
                            #     tags$input(id = paste0("textboxColumns",i), type = "text", class = "form-control shiny-bound-input shinyjs-resettable", value = columnNames[i]),
                            #     tags$span(class = "input-group-btn",
                            #               tags$button(type = "button", class = "btn btn-default action-button btn-danger", icon("times"), title = "Delete")
                            #               )
                            #     )
                            #

                            textInput(paste0("textboxColumns", i), paste("Column", i), columnNames[i])
                            # actionButton("test", "",icon = icon("times"), class = "btn-sm btn-danger")
                        )
                    }
                })
            })

            outputUI[[length(outputUI) + 1]] <- div(style = "clear:both;")

            outputUI[[length(outputUI) + 2]] <- actionButton("saveColumnNames", "Save")


            wellPanel(outputUI)
        }
    })

    observeEvent(input$saveColumnNames, {
        if (!is.null(myValues$mergedData)) {
            newColNames <- c()
            for (i in seq(ncol(myValues$mergedData))) {
                newColNames <- c(newColNames, input[[paste0("textboxColumns", i)]])
            }

            colnames(myValues$mergedData) <- newColNames

            updateCollapse(session, id = "editCols_collapse_panel", close = "editCols_panel")
        }
    })


    output$tab <- renderUI({
        tagList(
            h4(strong("Transcriptome Analysis (Optional):")),
            p("Start your analysis by launching the appropriate application for your data"),
            p("* If there are ", strong("NO replicates"), ", use DESeq2Shiny app for exploratory analysis"),
            p(strong("Your merged counts data will be automatically loaded")),
            fluidRow(
                column(8,
                    style = "margin-left: 20%;",
                    div(class = "BoxArea3 para", strong("Select Analysis Type:"))
                )
            ),
            fluidRow(
                column(8,
                    offset = 2,
                    div(class = "brace top")
                )
            ),
            fluidRow(
                column(
                    6,
                    tags$div(
                        class = "BoxArea3", style = "text-align: center;",
                        p(strong("Single-Cell RNA")),
                        a("Seurat Wizard", href = paste0("/SeuratWizard?countsdata=", encryptUrlParam(myValues$fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
                    )
                ),
                column(
                    6,
                    tags$div(
                        class = "BoxArea3", style = "text-align: center;",
                        p(strong("Bulk RNA")),
                        a("DESeq2Shiny", href = paste0("/deseq2shiny?countsdata=", encryptUrlParam(myValues$fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;"),
                        hr(),
                        a("START", href = paste0("/tsar_nasqar?countsdata=", encryptUrlParam(myValues$startAppFileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
                    )
                ),
                div(style = "clear:both;")
            )
        )
    })

    encryptUrlParam <- function(paramStr) {
        pubkeyHex <- read_file("public.txt")
        pubkey <- hex2bin(pubkeyHex)

        msg <- serialize(paramStr, NULL)
        ciphertext <- simple_encrypt(msg, pubkey)

        bin2hex(ciphertext)
    }

    output$contents <- renderDataTable(
        {
            # tmp <- analyzeDataReactive()
            # if(!is.null(tmp)) tmp$data

            if (!is.null(myValues$mergedData)) myValues$mergedData
        },
        options = list(scrollX = TRUE, pageLength = 10)
    )

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            write.csv(myValues$mergedData, con, row.names = FALSE)
        }
    )

    inputDataReactive <- reactive({
        inFile <- input$datafile
        if (is.null(inFile)) {
            return(NULL)
        }

        updateCollapse(session,
            id = "input_collapse_panel", open = "analysis_panel",
            style = list(
                "analysis_panel" = "primary",
                "data_panel" = "success"
            )
        )

        return(inFile)
    })


    analyzeDataReactive <-
        eventReactive(input$upload_data,
            ignoreNULL = FALSE,
            {
                removeNotification("errorNotify")

                inFile <- inputDataReactive()
                if (is.null(inFile)) {
                    return(NULL)
                }

                progress <- Progress$new(session, min = 0, max = 1)
                on.exit(progress$close())

                progress$set(message = "Merging files ...")

                files <- list()

                sep <- "\t"
                if (length(inFile$datapath) > 0) {
                    testSep <- read.csv(inFile$datapath[1], header = FALSE, sep = "\t")
                    if (ncol(testSep) < 2) {
                        sep <- ","
                    }
                } else {
                    return(NULL)
                }

                # remove zero size files
                inFile <- inFile[inFile$size != 0, ]


                ######
                suppressWarnings(
                    validate(need(
                        tryCatch(
                            {
                                total <- multmerge(inFile, sep, input$mergeType == "multiple")
                            },
                            error = function(e) {
                                myValues$status <- paste("Error: ", e$message, "\nMake sure your file(s) have the same dimensions")
                                updateTabsetPanel(session, "tabs", selected = "Output")
                                showNotification(id = "errorNotify", myValues$status, type = "error", duration = 20)
                                return(NULL)
                            }
                        ),
                        "Error merging files. Check!"
                    ))
                )



                if (input$addGeneNames) {
                    geneNames <- getNamesFromEnsembl(total[, 1], progress)

                    if (length(geneNames) != nrow(total)) {
                        myValues$status <- paste("Error converting gene names", "", "\nMake sure to select the correct genome/version")
                        showNotification(id = "errorNotify", myValues$status, type = "error", duration = 20)
                        updateTabsetPanel(session, "tabs", selected = "Output")

                        return(NULL)
                    }


                    if (input$geneNameColumn == "add") {
                        total <- as.data.frame(append(total, list(gene.names = geneNames), after = 1))
                    } else {
                        # total[,1] = list(gene.names= geneNames)

                        total[, 1] <- make.names(geneNames, unique = TRUE)
                        colnames(total)[1] <- "gene.names"
                    }
                }

                if (input$addOne) {
                    total[, !(names(total) %in% c("gene.ids", "gene.names"))] <- total[, !(names(total) %in% c("gene.ids", "gene.names"))] + 1
                }

                myValues$fileUrl <- UUIDgenerate()
                myValues$fileUrl <- paste0(tempdir(), "/", myValues$fileUrl, ".csv")

                updateTabsetPanel(session, "tabs", selected = "Output")

                total[, 1] <- as.character(total[, 1])
                myValues$mergedData <- total
                return(list("data" = total))
            }
        )

    observeEvent(
        {
            myValues$mergedData
        },
        {
            write.csv(myValues$mergedData, myValues$fileUrl, row.names = F)


            # this is specific to STARTapp. need to add _1 when no replicates are present
            myValues$startAppFileUrl <- gsub("\\.csv", "_startapp.csv", myValues$fileUrl)

            mergedCountsOnly <- myValues$mergedData[, -1]
            countsColStartIndex <- 2

            if (class(mergedCountsOnly[, 1]) != "integer") {
                mergedCountsOnly <- mergedCountsOnly[, -1]
                countsColStartIndex <- 3
            }

            # mergedColNames = colnames(mergedCountsOnly)
            #
            # containsUnderscoreReplNum= grepl('_[0-9]+$',mergedColNames)
            # if(!all(containsUnderscoreReplNum))
            # {
            #   #remove all underscores if present
            #   mergedColNames = gsub("_","",mergedColNames)
            #
            #   #add underscore 1
            #   mergedColNames = paste0(mergedColNames, "_1")
            #
            #   startappMergedDf = myValues$mergedData
            #   colnames(startappMergedDf)[seq(countsColStartIndex,ncol(startappMergedDf))] = mergedColNames
            #
            #   write.csv(startappMergedDf, myValues$startAppFileUrl, row.names = F)
            # }
            # else
            write.csv(myValues$mergedData, myValues$startAppFileUrl, row.names = F)
        }
    )

    multmerge <- function(inFiles, sep, isMultiple) {
        filenames <- inFiles$datapath


        datalist <- lapply(filenames, function(x) {
            fileContent <- read.csv(file = x, header = isMultiple, sep = sep)

            colnames(fileContent)[1] <- "gene.ids"
            fileContent <- fileContent[!grepl("__", fileContent[, 1]), ] # remove rows containing underscores

            # Sort by gene_id incase they are not sorted
            fileContent <- fileContent[order(fileContent[, 1]), ]

            fileContent
        })

        reduced <- Reduce(function(x, y) {
            merge(x, y, by = "gene.ids")
        }, datalist)



        if (!isMultiple) {
            samplenames <- unlist(lapply(inFiles$name, function(x) {
                tools::file_path_sans_ext(x)
            }))

            colnames(reduced) <- c("gene.ids", samplenames)
        }


        return(reduced)
    }



    getNamesFromEnsembl <- function(ensNames, progress) {
        # <- Progress$new(session, min=0, max=1)
        progress$set(value = 0.3)
        progress$set(message = "Adding gene names ...")


        # load("geneid2name.Rda")

        rda_file_path <- paste0("www/gene_names/", input$refGenome, ".Rda" )

        if (input$refGenome == "Other (not listed)") {
             geneid2name <- read.csv2(input$gtfMappingFile$datapath, sep = ",", colClasses = c("character", "character"))
        } else {
            load(rda_file_path)
        }

        #   ids<-bitr(ensNames, fromType = "ENSEMBL", toType = "SYMBOL", OrgDb="org.Ce.eg.db")
        #   ids$ENSEMBL <- ids$ENSEMBL
        # # df<-merge(df, ids[, c("WormBaseId", "SYMBOL")], by="WormBaseId",all.x = TRUE)



        # geneStartStr = as.character(ensNames[1])
        #
        # annoDb <- NULL
        # if(gdata::startsWith(geneStartStr, "ENSDAR",ignore.case=TRUE))
        #   annoDb= org.Dr.eg.db
        # else if(gdata::startsWith(geneStartStr, "ENSMUS",ignore.case=TRUE))
        #   annoDb <- org.Mm.eg.db
        # else if(gdata::startsWith(geneStartStr, "FB",ignore.case=TRUE))
        #   annoDb <- org.Dm.eg.db
        # else
        #   annoDb <- org.Hs.eg.db


        # Calculate the number of cores
        no_cores <- detectCores() - 1

        # Initiate cluster
        # cl <- makeCluster(no_cores)
        cl <- makeForkCluster(no_cores)

        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Started Renaming ", length(ensNames), " genes"))
        # levelsList = character(length(ensNames))
        levelsList <- parallel::parLapply(cl, ensNames, function(x) {
            # return(ids[geneid2name$gene_id == as.character(x),]$gene_name)
            return(geneid2name[geneid2name$gene_id == as.character(x), ]$gene_name)
        })

        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Finished renaming"))
        stopCluster(cl)

        progress$set(value = 0.8)

        flatList <- unlist(levelsList)

        progress$set(value = 1)
        return(flatList)

        # return(annoDb$SYMBOL)
    }


    output$filesUploaded <- reactive({
        return(!is.null(inputDataReactive()))
    })
    outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)

    output$filesMerged <- reactive({
        return(!is.null(analyzeDataReactive()))
    })
    outputOptions(output, "filesMerged", suspendWhenHidden = FALSE)

    output$columnNamesValid <- reactive({
        if (!is.null(myValues$mergedData)) {
            columnNames <- colnames(myValues$mergedData)[-1]

            valid <- all(grepl("^[[:alnum:]]+[_]*[[:digit:]]*$", columnNames, ignore.case = T))
            return(valid)
        }
        return(T)
    })
    outputOptions(output, "columnNamesValid", suspendWhenHidden = FALSE)


    observe({
        # Check if example selected, or if not then ask to upload a file.
        shiny::validate(
            need((input$data_file_type == "examplecounts") | ((!is.null(input$rdatafile)) | (!is.null(input$datafile))),
                message = "Please select a file"
            )
        )
        inFile <- input$datafile
    })
}

shinyApp(ui = ui, server = server)
