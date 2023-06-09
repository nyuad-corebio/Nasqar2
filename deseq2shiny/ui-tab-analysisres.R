tabItem(
    tabName = "resultsTab",
    fluidRow(
        column(
            11,
            h3(strong("Differential Expression Analysis")),
            wellPanel(
                conditionalPanel(
                    "output.noReplicates",
                    column(
                        12,
                        p('Experiments without replicates do not allow for estimation of the dispersion of counts around the expected value for each group, which is critical for differential expression analysis. If an experimental design is supplied which does not contain the necessary degrees of freedom for differential analysis, DESeq will provide a message to the user and follow the strategy outlined in Anders and Huber (2010) under the section "Working without replicates", wherein all the samples are considered as replicates of a single group for the estimation of dispersion. As noted in the reference above: "Some overestimation of the variance may be expected, which will make that approach conservative." Furthermore, "while one may not want to draw strong conclusions from such an analysis, it may still be useful for exploration and hypothesis generation."')
                    )
                ),
                conditionalPanel(
                    "!output.noReplicates",
                    radioButtons("resultNameOrFactor", "Select Contrast to extract results from DESeq:", choices = c("Factors", "Result Names")),
                    conditionalPanel(
                        "input.resultNameOrFactor == 'Result Names'",
                        column(
                            12,
                            selectizeInput("resultNamesInput", "Result Names", choices = c("Intercept"), multiple = T)
                        )
                    ),
                    conditionalPanel(
                        "input.resultNameOrFactor == 'Factors'",
                        column(
                            12,
                            selectizeInput("factorNameInput", "Factor Name", choices = c("Intercept"))
                        ),
                        column(3, selectInput("condition1", "Condition 1", choices = NULL)),
                        column(
                            1,
                            tags$div(
                                class = "form-group",
                                tags$label(" "),
                                p(strong("VS"))
                            )
                        ),
                        column(3, selectInput("condition2", "Condition 2", choices = NULL))
                    )
                ),
                column(
                    12,
                    tags$div(
                        class = "form-group",
                        actionButton("getDiffResVs", "Get Results", class = "btn btn-primary", style = "width:100%;")
                    )
                ),
                fluidRow(
                    column(
                        12,
                        strong("Conditions: "),
                        textOutput("factorsStr")
                    )
                ),
                div(style = "clear:both;")
            ),
            hr(),
            conditionalPanel(
                "output.comparisonComputed",
                tags$div(
                    class = "BoxArea2",
                    column(
                        12,
                        box(
                            title = "MA Plot Settings", solidHeader = T, status = "primary",
                            wellPanel(
                                sliderInput("alpha", "Adjusted p-value treshold", min = 0, max = 1, value = 0.1, step = 0.001),
                                numericInput("ylim",
                                    label = "Y Axis range abs value", min = 1, max = 10, value = 2
                                )
                            )
                        ),
                        box(
                            title = "MA Plot", solidHeader = T, status = "primary",
                            withSpinner(plotOutput(outputId = "maPlot"))
                        )
                    ),
                    htmlOutput("analysisRes_enrichGo"),
                    h4(p(class = "text-right", downloadButton("downloadVsCsv", "Download .csv", class = "btn btn-primary btn-sm"))),
                    h4(p(class = "text-right", actionButton("do", "mark it for further analysis", class = "btn btn-primary btn-sm"))),
                    dataTableOutput("savedFileList"),
                    withSpinner(dataTableOutput("comparisonData")),
                    div(style = "clear:both;")
                )
            )
        )
    )
)
