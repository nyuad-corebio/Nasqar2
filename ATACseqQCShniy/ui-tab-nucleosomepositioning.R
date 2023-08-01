tabItem(
    tabName = "nucleosomepositioning_tab",
    fluidRow(
        column(
            12,
            column(
                12,
                box(
                    title = "Upload bam file", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "selectSample1",
                    fluidRow(
                        column(
                            6,
                            selectInput("sel_sample_for_npositioning", "Select sample", choices = NULL, selected = NULL)
                        ),
                        column(
                            1,
                            selectInput("sel_chromosome",
                                label = "Chromosome", # or Ensembl ID",
                                choices = NULL,
                            )
                        ),
                        # Records
                        column(
                            3,
                            numericInput("record_count_value", span(
                            "Alignments to process",
                            span(
                                class = "TOOLTIP1",
                                `data-toggle` = "tooltip", `data-placement` = "right",
                                title = "A tooltip",
                                icon("question-circle","fa-solid")
                            )
                        ), value = 100, min = 0),
                        htmltags$script(
        "
        $('#record_count_value').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#record_count_value-label').html($('#record_count_value-label').text())}, 0.1);
        });
        "
    )
                        ),
                        column(
                            2,
                            textInput("motif_value", "Motif of interest", value = "CTCF")
                        ),
                        span(
                            "",
                            span(
                                class = "TOOLTIP1",
                                `data-toggle` = "tooltip", `data-placement` = "right",
                                title = "A tooltip",
                                icon("question-circle","fa-solid")
                            )
                        ),
                        # Integer type tags
                        column(
                            6,
                            selectizeInput("sel_tag_integer_type",
                            span(
                                "SAM integer type flags",
    #                             tags$a("SAM integer type flags", target = "_blank",
    #   href = "https://samtools.github.io/hts-specs/SAMtags.pdf"),
                               
                              tags$a(
                            span(
                                class = "TOOLTIP1",
                                `data-toggle` = "tooltip", `data-placement` = "right",
                                title = "For more information about sam flags click herel",
                                icon("question-circle","fa-solid")), target = "_blank",
                                href = "https://samtools.github.io/hts-specs/SAMtags.pdf"
                            )
                        )
                               , # or Ensembl ID",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                    placeholder =
                                        "Start typing to search for tags" # or ID',
                                )
                            )
                        ),
                        # Character type tags
                        column(
                            6,
                            selectizeInput("sel_tag_char_type",
                                label = "SAM charter type flags", # or Ensembl ID",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                    placeholder =
                                        "Start typing to search for a gene name/id" # or ID',
                                )
                            )
                        ),
                        column(
                            12,
                            actionButton("run_qc", "Run QC1",
                                class = "btn btn-success",
                                style = "width:100%;height:60px;"
                            )
                        )
                    )
                )
            ),
            column(12,
                    textOutput("empty_txt_output")),
            conditionalPanel(
                "output.task_done",
                column(
                    12,
                    column(
                        6,
                        wellPanel(
                            h3("Promoter/Transcript body (PT) score"),
                            withSpinner(plotOutput("plot_pt_score"))
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("Nucleosome Free Regions (NFR) score"),
                            withSpinner(plotOutput("plot_nfr_score"))
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("TSSR score"),
                            withSpinner(plotOutput("plot_tssre_score"))
                        )
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h3("Cumulative percentage of tag allocation in nucleosome-free and mononucleosome bam files."),
                        withSpinner(plotOutput("plotCumulativePercentage"))
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h3("Distribution of nucleosome-free and nucleosome-bound regions"),
                        withSpinner(plotOutput("plot_signals_tss"))
                    )
                ),

                # column(
                #     7,
                #     wellPanel(
                #         h3('plot_featureAlignedHeatmap'),
                #         withSpinner(plotOutput("plot_featureAlignedHeatmap")))
                # ),
                column(
                    7,
                    wellPanel(
                        h3("Distribution of nucleosome-free and nucleosome-bound regions"),
                        withSpinner(plotOutput("plot_signals")),
                        h5("Density plot showing distributions of reads from different bins around TSSs.
                    Black line, signal generated from nucleosome-free bin; red line, singal
                    from mononucleosome bin.")
                    )
                ),
                column(
                    7,
                    wellPanel(
                        h3("Footprints of DNA-binding factors."),
                        withSpinner(plotOutput("plot_Footprints"))
                    )
                ),
                column(
                    7,
                    wellPanel(
                        h3("Aggregate ATAC-seq Fragment Midpoint vs. Length for a given motif generated over binding sites
within the genome"),
                        withSpinner(plotOutput("plot_vp"))
                    )
                ),
                column(
                    7,
                    wellPanel(
                        h3("Distance of potential nucleosome dyad and the linear model for V"),
                        withSpinner(plotOutput("plot_distanceDyad"))
                    )
                )



                # actionButton("run_deseq2", "Run DESeq2",
                #              class = "btn btn-success",
                #              style = "width:100%;height:60px;"
                # ),
                # plotOutput("plot")
            )
        )
    )
)
