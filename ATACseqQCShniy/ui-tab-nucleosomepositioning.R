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
                    withSpinner(plotOutput("plot_pt_score")))
            ),
            column(
                6,
                wellPanel(
                    h3('Nucleosome Free Regions (NFR) score'),
                    withSpinner(plotOutput("plot_nfr_score")))
            ),
            column(
            6,
            wellPanel(
                h3('TSSR score'),
                withSpinner(plotOutput("plot_tssre_score")))
            )
            
        )),
        
        column(
            6,
            wellPanel(
                h3('Cumulative percentage of tag allocation in nucleosome-free and mononucleosome bam files.'),
                withSpinner(plotOutput("plotCumulativePercentage")))
        ),
            column(
            6,
            wellPanel(
                h3('Distribution of nucleosome-free and nucleosome-bound regions'),
                withSpinner(plotOutput("plot_signals_tss")))
        ),
        
        column(
            7,
            wellPanel(
                h3('plot_featureAlignedHeatmap'),
                withSpinner(plotOutput("plot_featureAlignedHeatmap")))
        ),
        column(
            7,
            wellPanel(
                h3('plot_signals'),
                withSpinner(plotOutput("plot_signals")))
        ),
        column(
            7,
            wellPanel(
                h3('plot_Footprints'),
                withSpinner(plotOutput("plot_Footprints")))
        ),
       
        column(
            7,
            wellPanel(
                h3('Aggregate ATAC-seq Fragment Midpoint vs. Length for a given motif generated over binding sites
within the genome'),
                withSpinner(plotOutput("plot_vp")))
        ),
        column(
            7,
            wellPanel(
                h3('Distance of potential nucleosome dyad and the linear model for V'),
                withSpinner(plotOutput("plot_distanceDyad")))
        )



            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        
    )
)
