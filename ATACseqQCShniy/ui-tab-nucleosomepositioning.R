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
                        # column(
                        #     3,
                        #     numericInput("record_count_value", span(
                        #         "Alignments to process",
                        #         span(
                        #             class = "TOOLTIP1",
                        #             `data-toggle` = "tooltip", `data-placement` = "right",
                        #             title = "A tooltip",
                        #             icon("question-circle", "fa-solid")
                        #         )
                        #     ), value = 100, min = 0),
                        #     htmltags$script(
                        #                                 "
                        #         $('#record_count_value').on('shiny:updateinput', function(e){
                        #             setTimeout(function(){$('#record_count_value-label').html($('#record_count_value-label').text())}, 0.1);
                        #         });
                        #         "
                        #     )
                        # ),
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
                                icon("question-circle", "fa-solid")
                            )
                        ),
                        # # Integer type tags
                        # column(
                        #     6,
                        #     selectizeInput("sel_tag_integer_type",
                        #         span(
                        #             "SAM integer type flags",
                        #             #
                        #             tags$a(
                        #                 span(
                        #                     class = "TOOLTIP1",
                        #                     `data-toggle` = "tooltip", `data-placement` = "right",
                        #                     title = "For more information about sam flags click herel",
                        #                     icon("question-circle", "fa-solid")
                        #                 ),
                        #                 target = "_blank",
                        #                 href = "https://samtools.github.io/hts-specs/SAMtags.pdf"
                        #             )
                        #         ),
                        #         # or Ensembl ID",
                        #         choices = NULL,
                        #         multiple = TRUE,
                        #         options = list(
                        #             placeholder =
                        #                 "Start typing to search for tags" # or ID',
                        #         )
                        #     )
                        # ),
                        # # Character type tags
                        # column(
                        #     6,
                        #     selectizeInput("sel_tag_char_type",
                        #         span(
                        #             "SAM charter type flags",
                        #             #
                        #             tags$a(
                        #                 span(
                        #                     class = "TOOLTIP1",
                        #                     `data-toggle` = "tooltip", `data-placement` = "right",
                        #                     title = "For more information about sam flags click herel",
                        #                     icon("question-circle", "fa-solid")
                        #                 ),
                        #                 target = "_blank",
                        #                 href = "https://samtools.github.io/hts-specs/SAMtags.pdf"
                        #             )
                        #         ),
                        #         choices = NULL,
                        #         multiple = TRUE,
                        #         options = list(
                        #             placeholder =
                        #                 "Start typing to search for a gene name/id" # or ID',
                        #         )
                        #     )
                        # ),
                        column(
                            12,
                            actionButton("run_qc", "Run QC",
                                class = "btn btn-success",
                                style = "width:100%;height:60px;"
                            )
                        )
                    )
                )
            ),
            column(
                12,
                textOutput("empty_txt_output")
            ),
            conditionalPanel(
                "output.task_done",
                column(
                    12,
                    column(
                        6,
                        wellPanel(
                            h3("Promoter/Transcript body (PT) score"),
                            withSpinner(plotOutput("plot_pt_score")),
                            h5("The Promoter/Transcript Body (PT) score is a metric used to assess signal is enriched 
                            in promoter regions compared to the transcript body regions. It helps in evaluating 
                            the presence of regulatory elements and potential gene expression regulation.
                            PT score is calculated as the coverage of promoter divided by the coverage of its transcript body")
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("Nucleosome Free Regions (NFR) score"),
                            withSpinner(plotOutput("plot_nfr_score")),
                            h5("NFR score is a ratio between cut signal adjacent to TSS and that flanking the corresponding TSS.
                            Each TSS window of 400 bp is first divided into 3 sub-regions: the most upstream 150 bp (n1), the most
                            downstream of 150 bp (n2), and the middle 100 bp (nf). Then the number of fragments with 5\' ends overlapping
                            each region are calculated for each TSS."),
                            h5("The NFR score for each TSS is calculated as NFR-score = log2(nf) - log2((n1+n2)/2)"),
                            h5("A plot can be generated with the NFR scores as Y-axis and the average signals of 400 bp window as X-axis, very like a MA plot
                            for gene expression data.")
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("TSSR score"),
                            withSpinner(plotOutput("plot_tssre_score")),
                            h5("TSS enrichment score is a raio between aggregate distribution of reads centered on TSSs and that flanking the corresponding TSSs.
                            TSS score = the depth of TSS (each 100bp window within 1000 bp each side) / the depth of end flanks (100bp each end).
                            TSSE score = max(mean(TSS score in each window))"),
                            h5("TSS enrichment score is calculated according to the definition given in the",a("link", href = "https://www.encodeproject.org/data-standards/terms/#enrichment")),
                            h5("Transcription start site (TSS) enrichment values are dependent on the reference files used;
                            cutoff values for high quality can be reffered ", a("here",href = "https://www.encodeproject.org/atac-seq/") )

                        )
                    )
                ),
                column(12,
                    wellPanel(
                        h3("Signal distribution around transcriptional start sites (TSSs)"),
                        h5("By averaging the signal across all active TSSs, we should observe that nucleosome-free
                        fragments are enriched at the TSSs, whereas the nucleosome-bound fragments should be enriched
                        both upstream and downstream of the active TSSs and display characteristic phasing of upstream
                        and downstream nucleosomes. Because ATAC-seq reads are concentrated at regions of open chromatin, 
                        users should see a strong nucleosome signal at the +1 nucleosome,
                        but the signal decreases at the +2, +3 and +4 nucleosomes.")
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
                        h3("Heatmap: Distributions of reads from different bins around TSSs"),
                        withSpinner(plotOutput("plot_tss_featureAlignedHeatmap"))
                        
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

                column(12,
                    wellPanel(
                        h3("Signal distribution around binding sites")
                       
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h3("Footprints of DNA-binding factors."),
                        # withSpinner(uiOutput("plots"))
                        # ,
                        withSpinner(plotOutput("plot_Footprints"))
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h3("Heatmap: Distributions of reads from different bins around binding sites"),
                        withSpinner(plotOutput("plot_binding_sites_featureAlignedHeatmap"))
                        
                    )
                ),
                column(
                    7,
                    wellPanel(
                        h3("Aggregate ATAC-seq Fragment Midpoint vs. Length for a given motif generated over binding sites within the genome"),
                        withSpinner(plotOutput("plot_vp"))
                    )
                ),
                column(
                    7,
                    wellPanel(
                        h3("Distance of potential nucleosome dyad and the linear model for V"),
                        withSpinner(plotOutput("plot_distanceDyad"))
                    )
                ),
                  column(
                        10,
                    tags$div(
                        class = "BoxArea2",
                        withSpinner(dataTableOutput("bamfilesTable"))
                    ),
                    downloadButton(
                        outputId = "download_bamfiles_btn",
                        label = "Download",
                        icon = icon("file-download")
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
