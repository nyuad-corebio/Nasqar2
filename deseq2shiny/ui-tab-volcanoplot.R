tabItem(
    tabName = "volcanoplotTab",
    fluidRow(
        column(
            11,
            box(
                title = "Volcano Plot", solidHeader = T, status = "primary", width = 12,
                fluidRow(
                    column(
                        6,
                        wellPanel(
                            uiOutput("select_ui"),
                            hr(),
                            conditionalPanel(
                                condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                                radioButtons("sig_genes_selection",
                                    label = h5("Significant Genes Selction"),
                                    choices = list("All significant genes" = 1, "Up regulated genes" = 2, "Down regulated genes" = 3),
                                    selected = 1
                                )
                            )
                        )
                    ),
                    column(
                        6,
                        conditionalPanel(
                            condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                            h4(strong("Threshold Settings:")),
                            wellPanel(
                                sliderInput("significance_threshold",
                                    label = h5("Significance threshold:"), min = 0,
                                    max = 100, value = 3
                                ),
                                sliderInput("log_fold_change_threshold",
                                    label = h5("Fold Change threshold:"), min = 0,
                                    max = 5, value = 3
                                )
                            )
                        )
                    )
                )
            ),
            hr(),
            div(style = "clear:both;"),
            conditionalPanel(
                condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                div(
                    class = "BoxArea6",
                    fluidRow(
                        column(12,
                            column(2,div()),
                            column(8,plotOutput("curve_plot",height = "100%")),
                            column(2,div())
                        ),
                        
                        div(style = "clear:both;"),
                        hr(),
                        column(12,div(
                            h3(textOutput("sig_genes_header")),
                            hr(),
                            withSpinner(
                                dataTableOutput("sig_gene_table"),

                                # htmlOutput("enrichGo_volcano")
                                # ,

                                # conditionalPanel(
                                #     condition = "input.gene_alias=='included'",
                                #     wellPanel(
                                #         radioButtons("volcano_sel_gene_type", "Gene enrichment by",
                                #             c(
                                #                 "gene.id" = "gene.id",
                                #                 "gene.name" = "gene.name"
                                #             ),
                                #             selected = "gene.id"
                                #         ),
                                #         div(style = "clear:both;")
                                #     )
                                # ),
                            )
                        ),
                        column(12,hr()),
                        column(12,
                            div(style = "clear:both;"),
                            fluidRow(
                                 conditionalPanel(condition = "input.gene_alias=='included'",
                                column(4,
                                   
                                    radioButtons("volcano_sel_gene_type", "Gene list by",
                                        c(
                                            "gene.id" = "gene.id",
                                            "gene.name" = "gene.name"
                                        ),
                                        selected = "gene.id"
                                        
                                    )
                                )),
                                column(4,
                                    radioButtons("volcano_input_genes_sep", "Genes separted by",
                                        c(
                                            "Comma" = ",",
                                            "Space" = " "
                                        ),
                                        selected = "Space"
                                    )
                                )
                                
                            ),
                            textAreaInput("volcano_gene_list", 'GeneList', rows = 3, width="100%")

                        ))
                    ),

                    
                )
            )
        )
    )
)
