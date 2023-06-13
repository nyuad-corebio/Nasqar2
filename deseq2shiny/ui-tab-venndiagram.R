tabItem(
    tabName = "venndiagramTab",
    fluidRow(
        box(
            title = "Venn Diagram", solidHeader = T, status = "primary", width = 12,
            fluidRow(
                column(
                    6,
                    wellPanel(
                        column(
                            12,
                            uiOutput("select_venn_ui")
                        ),
                        radioButtons("venn_sig_genes_selection",
                            label = h5("Significant Genes Selction"),
                            choices = list("All significant genes" = 1, "Up regulated genes" = 2, "Down regulated genes" = 3),
                            selected = 1
                        ),
                        actionButton("plotVenn", "Plot Venn  diagram"),
                        div(style = "clear:both;")
                    )
                ),
                column(
                    6,
                    h4(strong("Threshold Settings:")),
                    wellPanel(
                        fluidRow(
                            column(
                                12,
                                sliderInput("venn_significance_threshold",
                                    label = h5("Significance threshold:"), min = 0,
                                    max = 15, value = 3
                                )
                            ),
                            column(
                                12,
                                sliderInput("venn_log_fold_change_threshold",
                                    label = h5("Fold Change threshold:"), min = 0,
                                    max = 5, value = 3
                                )
                            )
                        ),
                        # column(2,
                        #        radioButtons("counttype","Y axis:",choices=c("counts","rlog","vst"))
                        #        ),
                        # column(2,
                        #        actionButton("genBoxplot","Generate Plot", class = "btn btn-primary", style = "width:100%;")
                        # ),
                        div(style = "clear:both;")
                    )
                )
            )
        )
    ),
    fluidRow(
        hr(),
        textOutput(outputId = "message"),
        conditionalPanel(
            condition = "output.panelStatus && input.select_avo_de_venn_files.length > 1",
            tags$div(
                class = "BoxArea2",
                column(
                    12,
                    plotOutput("vennDiagram")
                ),
                column(
                    12,
                    hr()
                ),
                column(
                    6,
                    DTOutput("gene_data_sets"),
                ),
                column(
                    12,
                    selectInput("select_expression", "Select set operations", choices = c())
                ),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.gene_data_sets",
                        textInput("venn_set_expression_input", "Enter the set expression", value = "", width = NULL, placeholder = NULL),
                    )
                ),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.gene_data_sets",
                        actionButton("evaluateExpression", "Evaluate Expression"),
                    )
                ),
                hr(),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.gene_data_sets",
                        h3("Expression Result")
                    )
                ),
                hr(),
                column(
                    12,
                    DTOutput("venn_expression_result")
                ),
                hr(),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.venn_expression_result",
                        InteractiveComplexHeatmapOutput("ht1",output_ui = htmlOutput("info"))
                    )
                    # plotOutput("heatMap1")htmlOutput("info1"),
                ),
                hr(),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.selected_genes",
                        h3("Selected portion from heatmap")
                        # plotOutput("heatMap1")htmlOutput("info1"),
                    )
                ),
                hr(),
                column(
                    12,
                    DTOutput("venn_diagram_heatmap_matrix_table")
                    # plotOutput("heatMap1")htmlOutput("info1"),
                ),
                hr(),
                column(12,
                    conditionalPanel(condition = "output.selected_genes",
                        column(
                            12,
                            conditionalPanel(
                                condition = "input.gene_alias=='included'",
                                column(4,
                                    radioButtons("venn_sel_gene_type", "Gene list by",
                                            c(
                                                "gene.id" = "gene.id",
                                                "gene.name" = "gene.name"
                                            ),
                                            selected = "gene.id"
                                    )
                                )
                            ),
                            column(4,
                                radioButtons("venn_input_genes_sep", "Genes separted by",
                                    c(
                                        "Comma" = ",",
                                        "Space" = " "
                                    ),
                                    selected = "Space"
                                )
                            ),
                            
                        ),
                        column(12,
                            textAreaInput("venn_gene_list", 'GeneList', rows = 3, width="100%")
                        )
                    )
                ),
      
                column(
                    4,
                    conditionalPanel(
                        condition = "output.venn_expression_result",
                        htmlOutput("info2")
                    )
                ),
                #   column(4,
                #        conditionalPanel( condition = "output.venn_expression_result",
                #         htmlOutput("enrichGo"))
                #  ),

                column(
                    12,
                    conditionalPanel(
                        condition = "output.venn_expression_result && output.selected_genes",
                        h3("Scatter plot of selected genes from heatmap")
                    )
                ),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.venn_expression_result && output.selected_genes",
                        plotOutput("scaterplot", height = "800px")
                    )
                ),
                # h4(p(class = "text-right",downloadButton('downloadBoxCsv','Download .csv', class = "btn btn-primary btn-sm"))),
                # withSpinner(dataTableOutput("boxplotData")),
                div(style = "clear:both;")
            )
        )
    )
)
