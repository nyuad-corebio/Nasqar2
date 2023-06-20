tabItem(
    tabName = "nucleosomepositioning_tab",
    fluidRow(
        column(12,
        
        column(
            12,
            box(
                title = "Upload bam file", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "selectSample1",
                fluidRow(
                    column(5,
                        selectInput("sel_sample_for_npositioning", "Select sample", choices = NULL, selected = NULL)
                    ),
                    column(3,
                        selectInput("sel_chromosome",
                            label = "Chromosome", # or Ensembl ID",
                            choices = NULL,
                         )
                    ),
                    column(3,
                       numericInput("record_count_value", "Records", value = 100, min = 0)       
                    ),
                    column(6,
                            selectizeInput("sel_tag_integer_type",
                                label = "Integer type tags", # or Ensembl ID",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                    placeholder =
                                        "Start typing to search for tags" # or ID',
                                )
                            )
                    ),
                    column(6,
                        selectizeInput("sel_tag_char_type",
                            label = "Character type tags", # or Ensembl ID",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(
                                placeholder =
                                    "Start typing to search for a gene name/id" # or ID',
                            )
                        )
                    ),
                    
                    column(12,
                    actionButton("run_qc", "Run QC1",
                                class = "btn btn-success",
                                style = "width:100%;height:60px;"
                            )
                    )
                )
            )
        ),
        column(
            12,
            column(
                6,
                wellPanel(
                    h3('Promoter/Transcript body (PT) score'),
                    plotOutput("plot_pt_score"))
            ),
            column(
                6,
                wellPanel(
                    h3('Nucleosome Free Regions (NFR) score'),
                    plotOutput("plot_nfr_score"))
            ),
            column(
            6,
            wellPanel(plotOutput("plot_tssre_score"))
            )
            
        )),
        
        column(
            6,
            wellPanel(plotOutput("plotCumulativePercentage"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_featureAlignedHeatmap"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_signals"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_Footprints"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_segmentation"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_vp"))
        ),
        column(
            7,
            wellPanel(plotOutput("plot_distanceDyad"))
        )



            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        
    )
)
