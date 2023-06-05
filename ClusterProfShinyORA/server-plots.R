output$upsetPlot <- renderPlot({
    withProgress(message = "Plotting upsetplot ...", {
        go_enrich <- enrichGoReactive()$go_enrich
        s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_upset", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_upset", max = nrow(go_enrich@result), min = 0)
        }
        upsetplot(go_enrich,
            n = input$showCategory_upset
        )
    })
})

output$barPlot <- renderPlot({
    withProgress(message = "Plotting barplot ...", {
        go_enrich <- enrichGoReactive()$go_enrich

        s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_bar", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_bar", max = nrow(go_enrich@result), min = 0)
        }

        barplot(go_enrich,
            drop = TRUE,
            showCategory = input$showCategory_bar,
            title = "GO Biological Pathways",
            font.size = 8
        )
    })
})


output$dotPlot <- renderPlot({
    withProgress(message = "Plotting dotplot ...", {
        go_enrich <- enrichGoReactive()$go_enrich
         s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_dot", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_dot", max = nrow(go_enrich@result), min = 0)
        }

        dotplot(go_enrich,
            # drop = TRUE,
            showCategory = input$showCategory_dot,
            # title = "GO Biological Pathways",
            font.size = 8
        )
    })
})

output$enrichPlotMap <- renderPlot({
    withProgress(message = "Plotting  enrichment map ...", {
        go_enrich <- enrichGoReactive()$go_enrich
        s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_enrichmap", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_enrichmap", max = nrow(go_enrich@result), min = 0)
        }

        emapplot(pairwise_termsim(go_enrich), showCategory = input$showCategory_enrichmap)
    })
})


output$goInducedGraph <- renderPlot({
    withProgress(message = "Plotting  induced GO DAG ...", {
        go_enrich <- enrichGoReactive()$go_enrich
        s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_goplot", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_goplot", max = nrow(go_enrich@result), min = 0)
        }


        goplot(go_enrich, showCategory = input$showCategory_goplot)
    })
})


output$cnetplot <- renderPlot({
    withProgress(message = "Plotting Gene-Concept Network ...", {
        go_enrich <- enrichGoReactive()$go_enrich
        s <- input$enrichGoTable_rows_selected
        if (length(s) > 0) {
            go_enrich <- filter(go_enrich, ID %in% go_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_cnet", max = nrow(go_enrich@result), min = 0, value = ifelse(nrow(go_enrich@result) > 0, nrow(go_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_cnet", max = nrow(go_enrich@result), min = 0)
        }
        cnetplot(go_enrich, categorySize = "pvalue", foldChange = myValues$gene_list, showCategory = input$showCategory_cnet)
    })
})

output$wordcloud <- renderWordcloud2({
    withProgress(message = "Plotting Wordcloud ...", {
        if (input$plotWordcloud) {
            isolate({
                if (input$wordcloudGoOrKegg == "enrichGo") {
                    go_enrich <- enrichGoReactive()$go_enrich
                    
                } else {
                    go_enrich <- enrichGoReactive()$kegg_enrich
                }

                wcdf <- read.table(text = go_enrich$GeneRatio, sep = "/")[1]
                wcdf$term <- go_enrich[, 2]
                # wc = wordcloud(words = wcdf$term, freq = wcdf$V1, scale=(c(2, .5)), colors=brewer.pal(8, "Dark2"), max.words = input$maxWords)

                wordsDF <- data.frame(word = go_enrich[, 2], freq = wcdf$V1)

                wordsDF <- wordsDF[order(-wordsDF$freq), ]

                wordsDF <- head(wordsDF, input$maxWords)

                wordcloud2(wordsDF, color = input$wordsColor, shape = input$wordShape, size = 0.1)
            })
        }
    })
})


### KEGG

output$barPlot_kegg <- renderPlot({
    withProgress(message = "Plotting barplot ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich
        
        s <- input$enrichKEGGTable_rows_selected
        if (length(s) > 0) {
            kegg_enrich <- filter(kegg_enrich, ID %in% kegg_enrich@result$ID[s])
            updateNumericInput(session, "showCategory_bar_kegg", max = nrow(kegg_enrich@result), min = 0, value = ifelse(nrow(kegg_enrich@result) > 0, nrow(kegg_enrich@result), 0))
        } else {
            updateNumericInput(session, "showCategory_bar_kegg", max = nrow(kegg_enrich@result), min = 0)
        }

        barplot(kegg_enrich,
            drop = TRUE,
            showCategory = input$showCategory_bar_kegg,
            title = "Enriched Pathways",
            font.size = 8
        )
    })
})


output$dotPlot_kegg <- renderPlot({
    withProgress(message = "Plotting dotplot ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich
        print("Plotting dotplot")
        s <- input$enrichKEGGTable_rows_selected
        print(s)
        if (s != NULL && length(s) > 0) {
            kegg_enrich <- filter(kegg_enrich, ID %in% kegg_enrich@result$ID[s])
            print(str(kegg_enrich))
        } 
        updateNumericInput(session, "showCategory_dot_kegg", max = nrow(kegg_enrich@result), min = 0)
        print(input$showCategory_dot_kegg)
        dotplot(kegg_enrich,
            # drop = TRUE,
            showCategory = input$showCategory_dot_kegg,
            title = "Enriched Pathways",
            font.size = 8
        )
    })
})

output$enrichPlotMap_kegg <- renderPlot({
    withProgress(message = "Plotting Enrichment Map ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich
        

        emapplot(pairwise_termsim(kegg_enrich), showCategory = input$showCategory_enrichmap_kegg)
    })
})


output$cnetplot_kegg <- renderPlot({
    withProgress(message = "Plotting Gene-Concept Network ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich
        s <- input$enrichKEGGTable_rows_selected
        if (length(s) > 0) {
            kegg_enrich <- filter(kegg_enrich, ID %in% kegg_enrich@result$ID[s])
        } 
        updateNumericInput(session, "showCategory_cnet_kegg", max = nrow(kegg_enrich@result), min = 0)
        cnetplot(kegg_enrich, categorySize = "pvalue", foldChange = myValues$kegg_gene_list, showCategory = input$showCategory_cnet_kegg)
    })
})


pathviewReactive <- eventReactive(input$generatePathview, {
    withProgress(message = "Plotting Pathview ...", {
        isolate({
            kegg_enrich <- enrichGoReactive()$kegg_enrich
          
            setProgress(value = 0.3, detail = paste0("Pathview ID ", input$pathwayIds, " ..."))
            dme <- pathview(gene.data = myValues$gene_list, pathway.id = input$pathwayIds, species = myValues$organismKegg, gene.idtype = input$geneid_type)
            file.copy(paste0(input$pathwayIds, ".pathview.png"), paste0("testimage"))

            setProgress(value = 0.7, detail = paste0("Pathview ID ", input$pathwayIds, " generating pdf ..."))
            dmePdf <- pathview(gene.data = myValues$gene_list, pathway.id = input$pathwayIds, species = myValues$organismKegg, gene.idtype = input$geneid_type, kegg.native = F)

            myValues$imagePath <- paste0(input$pathwayIds, ".pathview.")
            return(list(
                src = paste0("testimage"),
                filetype = "image/png",
                alt = "pathview image"
            ))
        })
    })
})

output$pathview_plot <- renderImage({
    return(pathviewReactive())
})

output$pathviewPlotsAvailable <-
    reactive({
        return(!is.null(pathviewReactive()))
    })
outputOptions(output, "pathviewPlotsAvailable", suspendWhenHidden = FALSE)

output$downloadPathviewPng <- downloadHandler(
    filename = function() {
        paste0(myValues$imagePath, "png")
    },
    content = function(file) {
        file.copy(paste0(getwd(), "/", myValues$imagePath, "png"), file)
    }
)

output$downloadPathviewPdf <- downloadHandler(
    filename = function() {
        paste0(myValues$imagePath, "pdf")
    },
    content = function(file) {
        file.copy(paste0(getwd(), "/", myValues$imagePath, "pdf"), file)
    }
)

output$wordcloud_kegg <- renderWordcloud2({
    withProgress(message = "Plotting Wordcloud ...", {
        kegg_enrich <- enrichGoReactive()$kegg_enrich


        wcdf <- read.table(text = kegg_enrich$GeneRatio, sep = "/")[1]
        wcdf$term <- kegg_enrich[, 2]
        # wc = wordcloud(words = wcdf$term, freq = wcdf$V1, scale=(c(2, .5)), colors=brewer.pal(8, "Dark2"), max.words = input$maxWords)

        wordsDF <- data.frame(word = kegg_enrich[, 2], freq = wcdf$V1)

        wordsDF <- wordsDF[order(-wordsDF$freq), ]

        wordsDF <- head(wordsDF, input$maxWords_kegg)
        # browser()
        wordcloud2(wordsDF, color = input$wordsColor_kegg, shape = input$wordShape_kegg, size = 0.1)
    })
})
