observe({
    # updateSelectizeInput(session,'sel_gene',
    #                      choices= rownames(myValues$dataCounts),
    #                      server=TRUE)
    # browser()
    tmpgroups <- colnames(myValues$DF)
    tmpgroups <- unlist(lapply(tmpgroups, function(x) {
        levels(myValues$DF[, x])
    }))

    updateSelectizeInput(session, "heat_group",
        choices = tmpgroups, selected = tmpgroups, server = T
    )
})

observe({
    req(input$genHeatmap > 0)
    logNormCounts <- heatmapReactive()$logNormCounts
     ht <- Heatmap(logNormCounts)
})

heatmapReactive <- reactive({
    if (input$genHeatmap > 0) {
        isolate({

        updateActionButton(session, "genHeatmap",
      label = "Generating heatmap...")
         
        shinyjs::disable("genHeatmap")
        

            logNormCounts <- log2((counts(myValues$dds, normalized = TRUE, replaced = FALSE) + .5))
            # %>%
            #   gather(gene, expression, (ncol(.)-length(input$sel_gene)+1):ncol(.))



            # vst = myValues$vstMat

            # selGroupSamples = row.names(myValues$DF[myValues$DF$Conditions %in% input$heat_group,])
            # logNormCounts = logNormCounts[,selGroupSamples]

            # vst = vst[,input$heat_group]

            myValues$heatmap_path <- paste0(tempdir(), "/", "heatmap-highres.pdf")

            if (!input$subsetGenes) {
                tmpsd <- apply(logNormCounts, 1, sd)

                selectGenes <- rownames(logNormCounts)[order(tmpsd, decreasing = TRUE)]
                selectGenes <- head(selectGenes, input$numGenes)

                genesNotFound <- NULL
            } else {
                genes <- unlist(strsplit(input$listPasteGenes, ","))

                genes <- gsub("^\\s+|\\s+$", "", genes)
                genes <- gsub("\\n+$|^\\n+", "", genes)
                genes <- gsub("^\"|\"$", "", genes)

                genes <- genes[genes != ""]

                print("heatmappp")
                print(genes)

                if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                    # get gene.ids from gene.name
                    genes <- myValues$geneids[genes, ]
                }
                print(genes)
                genesNotFound <- genes[!(genes %in% rownames(logNormCounts))]

                genes <- genes[!(genes %in% genesNotFound)]

                selectGenes <- genes
            }



            logNormCounts <- logNormCounts[selectGenes, ]
            if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                # get gene.ids from gene.name
                rownames(logNormCounts) <- myValues$genenames[selectGenes, ]
            }
            # print(logNormCounts)

            ht <- Heatmap(logNormCounts)
            ht <- draw(ht)
            makeInteractiveComplexHeatmap(input, output, session, ht, 
                click_action = heat_map_click_action, brush_action = heat_map_brush_action,"ht2"
            )
           

            #generateHeatmapPdf(logNormCounts, Rowv, annLegend, annCol)
            #  shinyjs::enable("genHeatmap")
            return(list("logNormCounts" = logNormCounts, "genesNotFound" = genesNotFound))
        })
    }

    #
})

output$heatmapPlot <- renderPlot({
    print("heatmapPlot")
    if (!is.null(heatmapReactive())) {
        print("logNormCounts")
        logNormCounts <- heatmapReactive()$logNormCounts
        genesNotFound <- heatmapReactive()$genesNotFound

        validate(
            # need( is.null(genesNotFound) || length(genesNotFound) < 1, message = "Some genes were not found!"),
            need(nrow(logNormCounts) > 1, message = "Need atleast 2 genes to plot!")
        )


        coldata <- colData(myValues$dds)
        coldata$sizeFactor <- NULL
        coldata$replaceable <- NULL

        if (input$no_replicates) {
            annLegend <- F
            Rowv <- NA
            annCol <- NULL
        } else {
            annLegend <- T
            Rowv <- NA

            annCols <- colnames(coldata)[!colnames(coldata) %in% c("sizeFactor", "replaceable")]
            annCol <- as.data.frame(coldata[, annCols])
        }

       
        # dev.off()

        print("aheatmap")
        print(annCol)

        p <- pheatmap(logNormCounts)
        # p <- aheatmap(logNormCounts,scale = "none",
        #               revC=TRUE,
        #               fontsize = 10,
        #               cexRow = 1.2,
        #               Rowv = Rowv,
        #               annLegend = annLegend,
        #               #color = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255),
        #               annCol = annCol
        #  )

        # print(p)
        return(p)
    }
})

heat_map_click_action <- function(df, output) {
    print('heat_map_click_action')
    shinyjs::enable("genHeatmap")
    updateActionButton(session, "genHeatmap",
      label = "Generate Plot")
    output[["heatmap_click"]] <- renderUI({
        if (!is.null(df)) {
            HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
        }
    })
}

heat_map_brush_action <- function(df, output) {
    input$evaluateExpression

    row_index <- unique(unlist(df$row_index))
    column_index <- unique(unlist(df$column_index))

    # if (!is.null(heatmapReactive())) {
    #     matrix <- heatmapReactive()$logNormCounts
    #     m <- matrix[row_index, column_index, drop = FALSE]
    #        output[["heatmap_matrix_table"]] <- DT::renderDataTable(
    #     {
    #         gene.id <- rownames(m)
    #         genes <- gene.id



    #         if (input$gene_alias == "included") {
    #             genes <- myValues$genenames[gene.id, ]
    #             gene.name <- genes
    #             m <- cbind(m, gene.name)
    #         }

    #         if (input$venn_sel_gene_type == "gene.id") {
    #             genes <- gene.id
    #         }
    #         isolate({
    #             myValues$selected_genes <- myValues$selected_genes + 1
    #             print("myValues$selected_genes")
    #             print(myValues$selected_genes)
    #         })

    #         updateTextAreaInput(session, "venn_gene_list", value = paste(genes, collapse = input$venn_input_genes_sep))

    #         return(m)
    #     },
    #     options = list(scrollX = TRUE, pageLength = 50)
    # )

            
    # }



   
 


}


generateHeatmapPdf <- function(logNormCounts, Rowv, annLegend, annCol) {
    p <- pheatmap(logNormCounts, filename = myValues$heatmap_path)
    # heatmaptmp = heatmap(logNormCounts,scale = "none",
    #          revC=TRUE,
    #          fontsize = 10,
    #          cexRow = 1.2,
    #          Rowv = Rowv,
    #          annLegend = annLegend,
    #          #color = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255),
    #          annCol = annCol,
    #          filename = myValues$heatmap_path
    # )
}

# output$heatmapHighResAvailable <- reactive({
#
#   if(is.null(heatmapReactive()))
#     return(F)
#
#   return(file.exists(myValues$heatmap_path))
# })
# outputOptions(output, 'heatmapHighResAvailable', suspendWhenHidden=FALSE)

output$downloadHighResHeatmap <- downloadHandler(
    filename = c("heatmap_highres.pdf"),
    content = function(file) {
        file.copy(myValues$heatmap_path, file)
    }
)

output$heatmapData <- renderDataTable(
    {
        if (!is.null(heatmapReactive())) {
            heatmapReactive()$logNormCounts
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

#
output$heatmapComputed <- reactive({
    print("heatmapComputed")
    return(!is.null(heatmapReactive()))
})
outputOptions(output, "heatmapComputed", suspendWhenHidden = FALSE)


output$downloadHeatmapCsv <- downloadHandler(
    filename = function() {
        "heatmap_data.csv"
    },
    content = function(file) {
        csv <- heatmapReactive()$logNormCounts

        write.csv(csv, file)
    }
)
