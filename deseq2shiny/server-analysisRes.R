observe({
    compareReactive()
})

compareReactive <- reactive({
    if (input$getDiffResVs > 0) {
        withProgress(message = "Getting DESeq results , please wait ...", {
            isolate({
                factorsStr <- "Intercept: no replicates"
                if (input$no_replicates) {
                    myValues$vsResults <- results(myValues$dds)
                } else {
                    if (input$resultNameOrFactor == "Result Names") {
                        validate(
                            need((length(input$resultNamesInput) > 0 & length(input$resultNamesInput) < 3), message = "Need to choose at least 1 (Max. 2)")
                        )
                        js$addStatusIcon("resultsTab", "loading")

                        myValues$vsResults <- results(myValues$dds, contrast = list(input$resultNamesInput))
                        factorsStr <- paste(list(input$resultNamesInput))
                    } else if (input$resultNameOrFactor == "Factors") {
                        validate(
                            need((input$condition1 != input$condition2), message = "condition 1 must be different from condition 2")
                        )
                        js$addStatusIcon("resultsTab", "loading")

                        myValues$vsResults <- results(myValues$dds, contrast = c(input$factorNameInput, input$condition1, input$condition2))
                        factorsStr <- paste(input$factorNameInput, " : ", input$condition1, input$condition2)
                    }
                }


                js$addStatusIcon("resultsTab", "done")

                return(list("results" = myValues$vsResults, "conditions" = factorsStr))
            })
        })
    }
})

output$maPlot <- renderPlot({
    if (!is.null(compareReactive())) {
        isolate({
            plotMA(compareReactive()$results, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
        })
    }
})



filelist <- reactiveValues()
filelist$file_list <- list()



observeEvent(input$do, {
    df <- as.data.frame(compareReactive()$results)
    df[is.na(df)] <- 0
    if (input$resultNameOrFactor == "Result Names") {
        filename <- paste(input$resultNamesInput, collapse = "_")
    } else {
        filename <- paste0(input$factorNameInput, input$condition1, "_vs_", input$condition2, ".csv")
    }

    csv <- myValues$vsResults
    file <- paste0(tempdir(), "/", filename)
    filelist$file_list[[filename]] <- file
    write.csv(csv, file, row.names = T)
    # print( names(filelist$file_list))
    Saved.Results <- names(filelist$file_list)
    output$savedFileList <- renderDataTable({
        data.frame(Saved.Results)
    })
    print(Saved.Results)
    shinyjs::show(selector = "a[data-value=\"volcanoplotTab\"]")

    if (length(Saved.Results) > 1) {
        shinyjs::show(selector = "a[data-value=\"venndiagramTab\"]")
    }
    shinyjs::show(selector = "a[data-value=\"resultsTab\"]")
})








output$comparisonData <- renderDataTable(
    {
        if (!is.null(compareReactive())) {
            df <- as.data.frame(compareReactive()$results)
            df[is.na(df)] <- 0
            df
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

output$factorsStr <- renderText({
    if (!is.null(compareReactive())) {
        return(compareReactive()$conditions)
    }

    return(NULL)
})



output$analysisRes_enrichGo <- renderUI({
    fileUrl <- UUIDgenerate()
    fileUrl <- paste0(tempdir(), "/", fileUrl, ".csv")
    csv <- myValues$vsResults

    write.csv(csv, file = fileUrl)
    return(tags$div(
        class = "BoxArea3", style = "text-align: center;",
        p(strong("ClusterProfShinyORA")),
        a("goEnrich", href = paste0("/ClusterProfShinyORA?countsdata=", encryptUrlParam(fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
    ))
})










output$downloadVsCsv <- downloadHandler(
    filename = function() {
        paste0(input$condition1, "_vs_", input$condition2, ".csv")
    },
    content = function(file) {
        csv <- myValues$vsResults

        write.csv(csv, file, row.names = T)
    }
)

output$comparisonComputed <- reactive({
    return(!is.null(myValues$vsResults))
})
outputOptions(output, "comparisonComputed", suspendWhenHidden = FALSE)

output$noReplicates <- reactive({
    return(input$no_replicates)
})
outputOptions(output, "noReplicates", suspendWhenHidden = FALSE)
