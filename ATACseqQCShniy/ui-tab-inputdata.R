tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload bam file", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # h4("Upload Gene Counts"),
                h4("(select .bam/.bai)"),
                radioButtons("data_file_type", "Use example file or upload your own data",
                    c(
                        "Upload bam File" = "upload_bam_file",
                        "Example bam Data" = "example_bam_file"
                    ),
                    selected = "countsFile"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='upload_bam_file'",
                    p(".csv/.txt counts file (tab or comma delimited)"),
                    fileInput("bam_files", "",
                        accept = c(
                            ".bai",
                            ".bam"
                        ), multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='example_bam_file'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://doi.org/10.1007/978-3-319-07212-8_3", target = "_blank", "this publication")
                    )
                )
            ),
            conditionalPanel(
                # "output.fileUploaded",
                condition = "input.data_file_type=='example_bam_file'",
                box(
                    title = "ATACseqQC  Parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "createGoBox", collapsed = T,
                    wellPanel(
                        column(
                            12,
                            selectInput("bs_genome_input", "Reference genome:", choices = NULL, selected = NULL)
                        ),
                        column(
                            12,
                            selectInput("tx_db_input", "TxDb:", choices = c(""), selected = NULL)
                        ),
                        column(
                            12,
                            selectInput("phast_cons_input", "PhastCons:", choices = NULL, selected = NULL)
                        ),
                        # tags$div(class = "clearBoth")
                    ),
                    actionButton("initGo", "RUN QC2", class = "btn-info", style = "width: 100%")
                )
            )
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        ),
        column(
            6, 
                tags$div(
                    class = "BoxArea2",
                    withSpinner(tableOutput('bam_samples_table'))
                )
                
        )
    )
)
