output$select_ui <- renderUI({
    selectInput("select_avo_de_file",
        label = h5("Select DE data"), selected = NULL,
        choices = c(c("Select data"), names(filelist$file_list))
    )
})

avo_data <- reactive({
    print("avao_dataaaa")
    req(input$select_avo_de_file)

    print("avao_dataaaa2")




    req(input$select_avo_de_file != "Select data")

    df <- read.csv(filelist$file_list[[input$select_avo_de_file]])



    df <- na.omit(df)
    zero_padj_row <- df$padj < 1e-310
    print(sum(zero_padj_row))

    if (sum(zero_padj_row) > 0) {
        df[zero_padj_row, ]$padj <- 1e-310
    }


    withProgress(message = "Making plot", value = 0, {
        incProgress(0.3, detail = paste("fetching gene sysmbols"))


        # df <- df[(df$pvalue < 1 / 10^as.numeric(input$significance_threshold)) & abs(df$log2FoldChange) > as.numeric(input$log_fold_change_threshold), ]

        df$minus_10_log_padj <- -log10(df$padj)

        colnames(df)[1] <- "gene.id"

        if (input$gene_alias == "included") {
            genes <- myValues$genenames[df$gene.id, ]
            gene.name <- genes
            df <- cbind(df, gene.name)
        }
    })



    return(df)
})


all_genes <- reactive({
    df <- avo_data()
    print(as.numeric(input$log_fold_change_threshold))
    df <- df[(df$padj < 1 / 10^as.numeric(input$significance_threshold)) & abs(df$log2FoldChange) > as.numeric(input$log_fold_change_threshold), ]
    # print(df)
    return(df)
})

high_sig_genes <- reactive({
    df <- all_genes()
    # print(df)
    return(df[df$log2FoldChange > as.numeric(input$log_fold_change_threshold), ])
})

low_sig_genes <- reactive({
    df <- all_genes()
    return(df[df$log2FoldChange < as.numeric(input$log_fold_change_threshold), ])
})


sig_genes <- reactive({
    if (input$sig_genes_selection == "1") {
        print("All genes")
        return(all_genes())
    }
    if (input$sig_genes_selection == "2") {
        print("High genes")
        return(high_sig_genes())
    }

    print("Low genes")
    return(low_sig_genes())
})


output$sig_gene_table <- renderDataTable(
    {
        fileUrl <- UUIDgenerate()
        fileUrl <- paste0(tempdir(), "/", fileUrl, ".csv")
        genes_df <- sig_genes()

        if (input$gene_alias == "included") {
            gene_names <- genes_df$gene.name
            if (input$volcano_sel_gene_type == "gene.id") {
                gene_names <- genes_df$gene.id
            }
            colnames <- colnames(genes_df)
            col_to_move <-  which(colnames == 'gene.name')
            index <- c(1:length(colnames))
            index <- index[-col_to_move]

            index <- c(index[1], col_to_move, index[2:length(index)])


            # Reorder the columns
            genes_df <- genes_df[index]
        } else {
            gene_names <- genes_df$gene.id
        }



        # write.csv(gene_names, file = fileUrl, row.names = FALSE)


        updateTextAreaInput(session, "volcano_gene_list", value = paste(gene_names, collapse = input$volcano_input_genes_sep))

        # output[["volcano_gene_list_ouput"]] <- renderUI({
        #           textAreaInput("volcano_gene_list", 'GeneList', rows = 3)
        # })

        # output[["enrichGo_volcano"]] <-   renderUI({

        #       tags$div(class = "BoxArea7", style = "text-align: center;",
        #                     p(strong("ClusterProfShinyORA")),
        #                     a("goEnrich", href=paste0("/ClusterProfShinyORA?gene_names=", encryptUrlParam(fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
        #                     )
        # })


        return(genes_df)
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

output$sig_genes_header <- renderText({
    req(input$select_avo_de_file != "Select data")
    # print(length(all_genes()))
    if (input$sig_genes_selection == "1") {
        return(paste0("All significant genes: ", nrow(all_genes())))
    }
    if (input$sig_genes_selection == "2") {
        return(paste0("Up regulated genes: ", nrow(high_sig_genes())))
    }

    print("Low genes")
    return(paste0("Down regulated genes: ", nrow(low_sig_genes())))
})



output$curve_plot <- renderPlot(
    {
        print("curve_plot")

        df <- avo_data()

        print(df)

        if (input$gene_alias == "included") {
            return(EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file), subtitle = "",
                lab = df$gene.name, x = "log2FoldChange", y = "padj",
                pCutoff = 1 / 10^as.numeric(input$significance_threshold),
                FCcutoff = input$log_fold_change_threshold
            ))
        } else {
            return(EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file), subtitle = "",
                lab = df$gene.id, x = "log2FoldChange", y = "padj",
                pCutoff = 1 / 10^as.numeric(input$significance_threshold),
                FCcutoff = input$log_fold_change_threshold
            ))
        }
    },
    height = 600
)
