encryptUrlParam <- function(paramStr) {
    # pubkeyHex <- read_file("public.txt")
    pubkeyHex <- "42b3781d6907cd426b9c05cac7155cce15bb9385a602716f619529485dab6c28"
    pubkey <- hex2bin(pubkeyHex)

    msg <- serialize(paramStr, NULL)
    ciphertext <- simple_encrypt(msg, pubkey)

    bin2hex(ciphertext)
}


expression_set_data <- reactive({
    input$evaluateExpression

    # print('expression_set_data')
    infix_expression <- isolate(input$venn_set_expression_input)
    postfix_expression <- infix_to_postfix(infix_expression)
    # print(postfix_expression)
    genes <- postfix_eval(postfix_expression)

    return(genes)
})
output$select_venn_ui <- renderUI({
    selectInput("select_avo_de_venn_files",
        label = h5("Select DE data"), selected = NULL, multiple = TRUE,
        choices = names(filelist$file_list)
    )
})




heatmap_matrix <- reactive({
    print("heatmap_matrix")
    input$evaluateExpression

    req(isolate(input$venn_set_expression_input))

    req(length(isolate(input$select_avo_de_venn_files)) > 1)


    genes <- expression_set_data()


    common_genes <- genes[[1]]


    # print(input$avo_de_file)
    df_list <- c()
    namelist <- c()
    j <- 1

    for (f in isolate(input$select_avo_de_venn_files)) {
        print(f)
        print(filelist$file_list[[f]])
        df <- read.csv(filelist$file_list[[f]])
        df <- df[df$X %in% common_genes, ]
        df_list[[f]] <- df$log2FoldChange
    }

    # for(f in input$select_avo_de_venn_files){
    #
    #   i <- 1
    #   for(avo_file in input$avo_de_file$name){
    #     #print(input$avo_de_file$datapath)
    #     df <-  NULL
    #     print('aaa')
    #     print(f)
    #     print(avo_file)
    #     print('bbb')
    #     if (f == avo_file){
    #       df <- read.csv(input$avo_de_file$datapath[[i]])
    #       df <-  df[df$X %in% common_genes,]
    #
    #       df_list[[f]] <- df$log2FoldChange
    #       namelist[j] <-f
    #       j<-j+1
    #       break
    #
    #     }
    #     i <- i + 1
    #   }
    # }


    d <- data.frame(df_list)
    colnames(d) <- LETTERS[1:length(df_list)]
    # print('heatmap plot')


    rownames(d) <- common_genes

    # return (heatmaply(d, k_row = 3, k_col = 2,dendrogram="row", label_names= c("colum", "row", "va")))

    # pheatmap(d, cluster_cols=F)




    return(data.matrix(d))
})



click_action <- function(df, output) {
    output[["info"]] <- renderUI({
        if (!is.null(df)) {
            HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
        }
    })
}



output$selected_genes <- reactive({
    print("selected_genes")
    print(myValues$selected_genes)
    return(myValues$selected_genes > 1)
})

outputOptions(output, "selected_genes", suspendWhenHidden = FALSE)



selected_matrix <- reactiveValues()

brush_action <- function(df, output) {
    input$evaluateExpression

    row_index <- unique(unlist(df$row_index))
    column_index <- unique(unlist(df$column_index))
    matrix <- isolate(heatmap_matrix())



    m <- matrix[row_index, column_index, drop = FALSE]
    selected_matrix$matrix <- m
    output[["venn_diagram_heatmap_matrix_table"]] <- DT::renderDataTable(
        {
            gene.id <- rownames(m)
            genes <- gene.id



            if (input$gene_alias == "included") {
                genes <- myValues$genenames[gene.id, ]
                gene.name <- genes
                m <- cbind(m, gene.name)
            }

            if (input$venn_sel_gene_type == "gene.id") {
                genes <- gene.id
            }
            isolate({
                myValues$selected_genes <- myValues$selected_genes + 1
                print("myValues$selected_genes")
                print(myValues$selected_genes)
            })

            updateTextAreaInput(session, "venn_gene_list", value = paste(genes, collapse = input$venn_input_genes_sep))

            return(m)
        },
        options = list(scrollX = TRUE, pageLength = 50)
    )

    #  renderUI({




    # print(genes)
    # fileUrl <- UUIDgenerate()
    # fileUrl <- paste0(tempdir(),'/', fileUrl,'.csv')


    # output[["info2"]] <- renderUI({
    #     fileUrl <- UUIDgenerate()
    #     fileUrl <- paste0(tempdir(),'/', fileUrl,'.csv')

    #         # common_genes<-myValues$genenames[common_genes,]

    #     write.csv(genes, file = fileUrl, row.names = FALSE)


    #     output[["enrichGo"]] <-   renderUI({

    #         tags$div(class = "BoxArea3", style = "text-align: center;",
    #                 p(strong("ClusterProfShinyORA")),
    #                 a("goEnrich", href=paste0("/ClusterProfShinyORA?gene_names=", encryptUrlParam(fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;"))
    #     })

    #     return(textInput("genes", "Selected Genes", genes))
    # })
    #  if (!is.null(df)) {

    # return(HTML(kable_styling(kbl(m, digits = 2, format = "html"), full_width = FALSE, position = "left")))
    # }
    #     return(DT::renderDataTable({
    #        gene.id <- rownames(m)
    #         genes <- gene.id



    #           if(input$gene_alias == 'included'){
    #             genes <- myValues$genenames[gene.id,]
    #             gene.name <- genes
    #             m <- cbind(m, gene.name )
    #          }

    #               if(input$venn_sel_gene_type == 'gene.id'){
    #             genes <- gene.id

    #         }

    #         return(m)
    #     },options = list(scrollX = TRUE, pageLength = 50)))
    # })

    # DT::renderDataTable({

    #         m
    #     },options = list(scrollX = TRUE, pageLength = 50))



    # output[["info"]] <- renderUI({
    #     if (!is.null(df)) {


    #         HTML(kable_styling(kbl(m, digits = 2, format = "html"), full_width = FALSE, position = "left"))
    #     }
    # })
    # output[["info1"]] <- renderUI({
    #     DT::renderDataTable({

    #         m
    #     })
    # })





    # print('fileUrl')
    # print(fileUrl)
}











infix_to_postfix <- function(infix) {
    # indix <- "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
    operators <- c("+", "-", "*")
    precedence <- c(1, 1, 1)
    precedence1 <- c(1, 1, 1)
    # infix_tokens <- unlist(strsplit(infix, " "))
    infix <- gsub("\\s", "", infix)
    infix_tokens <- unlist(strsplit(infix, ""))
    stack <- c()
    result <- c()

    # print(infix_tokens)
    for (token in infix_tokens) {
        # print(token)
        # print(operators)
        if (token %in% operators) {
            if (length(stack) > 0 && (stack[length(stack)] %in% operators) && is.logical(precedence[precedence == stack[length(stack)]] >= precedence[precedence == token])) {
                # print(length(stack))
                result <- c(result, stack[length(stack)])
                stack <- stack[-length(stack)]
            }
            stack <- c(stack, token)
        } else if (token == "(") {
            stack <- c(stack, token)
        } else if (token == ")") {
            while (length(stack) > 0 && stack[length(stack)] != "(") {
                result <- c(result, stack[length(stack)])
                stack <- stack[-length(stack)]
            }
            if (length(stack) > 0 && stack[length(stack)] == "(") {
                stack <- stack[-length(stack)]
            } else {
                stop("Unmatched parentheses.")
            }
        } else {
            result <- c(result, token)
        }
    }

    while (length(stack) > 0) {
        if (stack[length(stack)] %in% operators) {
            result <- c(result, stack[length(stack)])
        } else {
            stop("Unmatched parentheses.")
        }
        stack <- stack[-length(stack)]
    }

    paste(result, collapse = " ")
}
postfix_eval <- function(postfix) {
    stack <- list()
    operators <- c("+", "-", "*", "/", "^")

    n_postfix <- list()

    d <- avo_venn_frames_data()
    df_list <- d[[1]]


    i <- 1

    for (token in unlist(strsplit(postfix, " "))) {
        if (length(token) == 1 && token %in% operators) {
            n_postfix[[i]] <- token
        } else {
            n_postfix[[i]] <- df_list[[token]]
        }


        i <- i + 1
    }

    j <- 1
    # print(n_postfix)
    for (token in n_postfix) {
        # print(token)
        if (length(token) == 1 && token %in% operators) {
            if (length(stack) < 2) {
                # print(stack)
                stop("Invalid postfix expression1.")
            }
            operand2 <- stack[[length(stack)]]
            stack <- stack[-length(stack)]
            operand1 <- stack[[length(stack)]]
            stack <- stack[-length(stack)]
            j <- j - 2
            result <- switch(token,
                "+" = union(operand1, operand2),
                "-" = setdiff(operand1, operand2),
                "*" = intersect(operand1, operand2)
            )
            stack[[j]] <- result
        } else {
            stack[[j]] <- token
        }
        j <- j + 1
    }
    # print(stack)
    if (length(stack) != 1) {
        stop("Invalid postfix expression.3")
    }

    stack[1]
}



output$panelStatus <- reactive({
    input$plotVenn > 0
})

outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)


observeEvent(input$evaluateExpression, {
    output$venn_expression_result <- renderDataTable(
        {
            print("venn_expression_result")
            matrix <- heatmap_matrix()
            print("matrix")
            print(matrix)
            print(nrow(matrix))
            req(nrow(matrix) > 0)
            # ht <- Heatmap(matrix, show_row_names = FALSE, show_column_names = FALSE)
            ht <- Heatmap(matrix)

            ht <- draw(ht)


            req(length(isolate(input$select_avo_de_venn_files)) > 1)

            req(isolate(input$venn_set_expression_input))

            genes <- expression_set_data()
            req(length(genes[1]) > 0)
            print("makeInteractiveComplexHeatmap start")
            makeInteractiveComplexHeatmap(input, output, session, ht, 
                click_action = click_action, brush_action = brush_action,"ht1"
            )
            print("makeInteractiveComplexHeatmap stop")



            values <- list()
            common_genes <- genes[[1]]
            values[["gene.id"]] <- common_genes


            if (input$gene_alias == "included") {
                values[["gene.name"]] <- myValues$genenames[common_genes, ]
            }

            tagnames <- LETTERS[1:length(isolate(input$select_avo_de_venn_files))]
            print(tagnames)
            df_list <- list()
            j <- 1
            for (f in names(filelist$file_list)) {
                print(f)
                print(filelist$file_list[[f]])
                df <- read.csv(filelist$file_list[[f]])
                df[is.na(df)] <- 0

                # df <- na.omit(df)
                df <- df[df$X %in% common_genes, ]
                # df <- na.omit(df)
                print(dim(df))
                values[[paste0(tagnames[j], ".logFC")]] <- df$log2FoldChange
                j <- j + 1
            }

            #
            #   i <- 1
            #   for(avo_file in input$avo_de_file$name){
            #     #print(input$avo_de_file$datapath)
            #     df <-  NULL
            #
            #     if (f == avo_file){
            #       df <- read.csv(input$avo_de_file$datapath[[i]])
            #       df <-  df[df$X %in% common_genes,]
            #       values[[paste0(tagnames[j],'.logFC')]]  <- df$log2FoldChange
            #
            #       j<-j+1
            #       break
            #
            #     }
            #     i <- i + 1
            #   }
            # }

            df <- data.frame(values)

            print(colnames(df))


            # wormBaseId <- df$WormBaseId
            # ids<-bitr(wormBaseId, fromType = "ENSEMBL", toType = "SYMBOL", OrgDb="org.Ce.eg.db")
            # ids$WormBaseId <- ids$ENSEMBL
            #
            # df<-merge(df, ids[, c("WormBaseId", "SYMBOL")], by="WormBaseId",all.x = TRUE)
            #
            #
            # incProgress(0.6, detail = paste("fetching gene ENTREZID"))
            #
            # ids<-bitr(wormBaseId, fromType = "ENSEMBL", toType = "GENENAME", OrgDb="org.Ce.eg.db")
            # ids$WormBaseId <- ids$ENSEMBL
            # df<-merge(df, ids[, c("WormBaseId", "GENENAME")], by="WormBaseId",all.x = TRUE)
            #
            # incProgress(0.6, detail = paste("fetching gene KEGG ID"))
            #
            # ids<-bitr(wormBaseId, fromType = "ENSEMBL", toType = "ENTREZID", OrgDb="org.Ce.eg.db")
            # ids$WormBaseId <- ids$ENSEMBL
            # df<-merge(df, ids[, c("WormBaseId", "ENTREZID")], by="WormBaseId",all.x = TRUE)
            #
            # kegg_ids <- bitr_kegg(ids$ENTREZID, fromType="ncbi-geneid", toType='kegg', organism='cel')
            # kegg_ids$ENTREZID<-kegg_ids$`ncbi-geneid`
            #
            # df<-merge(df, kegg_ids[, c("ENTREZID", "kegg")], by="ENTREZID",all.x = TRUE)
            DT::datatable(df)
        },
        options = list(scrollX = TRUE, pageLength = 5)
    )
    output$scaterplot <- renderPlot({
        print("scaterplot")
        venn_significance_threshold <- isolate(input$venn_significance_threshold)

        venn_log_fold_change_threshold <- isolate(input$venn_log_fold_change_threshold)
        req(length(input$select_avo_de_venn_files) > 1)

        genes <- expression_set_data()


        common_genes <- genes[[1]]


        j <- 1
        df_list <- list()
        plotlist <- list()
        for (f in names(filelist$file_list)) {
            print(f)
            print(filelist$file_list[[f]])
            df <- read.csv(filelist$file_list[[f]])
            df[is.na(df)] <- 0
            df <- df[df$X %in% common_genes, ]
            # df <- df[ df$padj > 0, ]
            df <- df[(df$padj < 1 / 10^as.numeric(venn_significance_threshold)) & abs(df$log2FoldChange) > as.numeric(venn_log_fold_change_threshold), ]
            df_list[[f]] <- df
        }


        # for(f in input$select_avo_de_venn_files){
        #
        #   i <- 1
        #   for(avo_file in input$avo_de_file$name){
        #     #print(input$avo_de_file$datapath)
        #     df <-  NULL
        #
        #     if (f == avo_file){
        #       df <- read.csv(input$avo_de_file$datapath[[i]])
        #       df <-  df[df$X %in% common_genes,]
        #       df <- na.omit(df)
        #       df <- df[df$pvalue < 0.5 & df$pvalue > 0 ,]
        #       df_list[[f]] <- df
        #
        #       j<-j+1
        #       break
        #
        #     }
        #     i <- i + 1
        #   }
        # }
        c <- combn(input$select_avo_de_venn_files, 2)

        for (i in 1:dim(c)[2]) {
            pair_names <- c[, i]
            # print(df_list[[pair_names[1]]])
            mergedfile <- merge(df_list[[pair_names[1]]], df_list[[pair_names[2]]], by = "X", all = T)
            mergedfile <- na.omit(mergedfile)
            all_plot <- ggplot(mergedfile, aes(x = log2FoldChange.x, y = log2FoldChange.y)) +
                geom_point(size = 2, shape = 19) +
                theme_minimal() +
                # geom_smooth(method = "lm", se = FALSE) + stat_cor() +
                sm_statCorr(
                    color = "#0f993d", corr_method = "spearman",
                    linetype = "dashed"
                ) +
                coord_fixed() +
                geom_vline(xintercept = 0) +
                geom_hline(yintercept = 0) +
                scale_y_continuous(name = pair_names[1], limits = c(-15, 15)) + # play with margin
                scale_x_continuous(name = pair_names[2], limits = c(-18, 18)) + # play with margin
                scale_color_manual(values = c("#660066", "#33ffcc", "#FF6633"))
            plotlist[[i]] <- all_plot
        }



        grid.arrange(arrangeGrob(grobs = plotlist, ncol = 2, padding = unit(10, "line")))
    })






    output$heatMap1 <- renderPlot({
        print("heatMap1")
        req(input$venn_set_expression_input)

        req(length(input$select_avo_de_venn_files) > 1)


        genes <- expression_set_data()


        common_genes <- genes[[1]]


        # print(input$avo_de_file)
        df_list <- c()
        namelist <- c()
        j <- 1

        for (f in input$select_avo_de_venn_files) {
            print(f)
            print(filelist$file_list[[f]])
            df <- read.csv(filelist$file_list[[f]])
            df <- df[df$X %in% common_genes, ]
            df_list[[f]] <- df$log2FoldChange
        }

        # for(f in input$select_avo_de_venn_files){
        #
        #   i <- 1
        #   for(avo_file in input$avo_de_file$name){
        #     #print(input$avo_de_file$datapath)
        #     df <-  NULL
        #     print('aaa')
        #     print(f)
        #     print(avo_file)
        #     print('bbb')
        #     if (f == avo_file){
        #       df <- read.csv(input$avo_de_file$datapath[[i]])
        #       df <-  df[df$X %in% common_genes,]
        #
        #       df_list[[f]] <- df$log2FoldChange
        #       namelist[j] <-f
        #       j<-j+1
        #       break
        #
        #     }
        #     i <- i + 1
        #   }
        # }


        d <- data.frame(df_list)
        colnames(d) <- LETTERS[1:length(df_list)]
        # print('heatmap plot')

        if (input$gene_alias == "included" && input$venn_sel_gene_type == "gene.name") {
            common_genes <- myValues$genenames[common_genes]
        }

        rownames(d) <- common_genes

        # return (heatmaply(d, k_row = 3, k_col = 2,dendrogram="row", label_names= c("colum", "row", "va")))

        # pheatmap(d, cluster_cols=F)
        pheatmap(data.matrix(d))
    })
})

# output$gene_data_sets <- DT::renderDataTable({

#     print('gene_data_sets')
#     d <- avo_venn_frames_data()

#     df <- data.frame("File Name" = d[[2]], "Label" = names(d[[1]]))
#     DT::datatable(df)
# },options = list(scrollX = TRUE, pageLength = 1))




avo_venn_frames_data <- reactive({
    print("avo_venn_frames_data")
    input$plotVenn

    # req(filelist$file_list)
    print("avo_venn_frames_data")
    select_avo_de_venn_files <- isolate(input$select_avo_de_venn_files)
    venn_significance_threshold <- isolate(input$venn_significance_threshold)
    venn_log_fold_change_threshold <- isolate(input$venn_log_fold_change_threshold)
    req(length(select_avo_de_venn_files) > 1)




    # print(input$avo_de_file)
    df_list <- c()
    namelist <- c()
    j <- 1

    for (f in select_avo_de_venn_files) {
        # print(f)
        # print(filelist$file_list[[f]])
        i <- 1
        df <- read.csv(filelist$file_list[[f]])
        df <- na.omit(df)
        # df <- df[df$padj > 0, ]
        df <- df[(df$padj < 1 / 10^as.numeric(venn_significance_threshold)) & abs(df$log2FoldChange) > as.numeric(venn_log_fold_change_threshold), ]
        if (input$venn_sig_genes_selection == "1") {

        }



        if (input$venn_sig_genes_selection == "2") {
            df <- df[df$log2FoldChange > as.numeric(venn_log_fold_change_threshold), ]
        }


        if (input$venn_sig_genes_selection == "3") {
            df <- df[df$log2FoldChange < as.numeric(venn_log_fold_change_threshold), ]
        }

        colnames(df)[1] <- "gene.id"
        # print(df)

        df_list[[j]] <- df$gene.id
        namelist[j] <- f
        j <- j + 1
    }



    names(df_list) <- LETTERS[1:length(df_list)]
    print(names(df_list))

    return(list(df_list, namelist))
})


generateBinaryMatrix <- function(set_names) {
    bits <- length(set_names)
    values <- c(0, 1)
    combinations <- expand.grid(replicate(bits, values, simplify = FALSE))
    colnames(combinations) <- set_names
    print(combinations)
    sortedCombinations <- combinations[do.call(order, combinations), ]
    rownames(sortedCombinations) <- apply(sortedCombinations, 1, function(row) {
        selectedColumns <- colnames(sortedCombinations)[row == 1]
        unselectedColumns <- colnames(sortedCombinations)[row == 0]
        if (length(selectedColumns) != 0) {
            if (length(unselectedColumns) == 0) {
                v <- paste0(selectedColumns, collapse = "*")
            } else {
                v <- paste0(c(paste0("(", paste0(selectedColumns, collapse = "*"), ")"), paste0("(", paste0(unselectedColumns, collapse = "+"), ")")), collapse = "-")
            }
        } else {
            v <- ""
        }

        print(v)
        return(v)
    })
    return(sortedCombinations)
}

observeEvent(input$select_expression, {
    updateTextInput(session, "venn_set_expression_input", value = input$select_expression)
})

output$gene_data_sets <- DT::renderDataTable(
    {
        print("gene_data_sets")
        d <- avo_venn_frames_data()

        df <- data.frame("File Name" = d[[2]], "Label" = names(d[[1]]))
        set_names <- names(d[[1]])

        combinations <- generateBinaryMatrix(set_names)
        set_expressions <- rownames(combinations[-1, ])
        updateSelectInput(session, "select_expression",
            choices = set_expressions,
            selected = set_expressions[length(set_expressions)][]
        )



        DT::datatable(df)
    },
    options = list(scrollX = TRUE)
)



observeEvent(input$plotVenn, {
    print("observeEvent")
    output$vennDiagram <- renderPlot({
        print("draw vennDiagram")
        a <- avo_venn_frames_data()
        df_list <- a[[1]]

        print(length(df_list))
        # oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
        # geneNames <- replicate(1000, oneName())

        # GroupA <- sample(geneNames, 400, replace=FALSE)
        # GroupB <- sample(geneNames, 750, replace=FALSE)
        # GroupC <- sample(geneNames, 250, replace=FALSE)
        # GroupD <- sample(geneNames, 300, replace=FALSE)

        # v1 <- venn.diagram(list(A=GroupA, B=GroupB, C=GroupC, D=GroupD), filename=NULL, fill=rainbow(4))
        v1 <- venn.diagram(df_list, filename = NULL, fill = rainbow(length(df_list)))

        print(v1)
        grid.newpage()
        grid.draw(v1)
    })
})
