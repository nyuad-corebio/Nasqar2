## ==================================================================================== ##
# START Shiny App for analysis and visualization of transcriptome data.
# Copyright (C) 2016  Jessica Minnier
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# You may contact the author of this code, Jessica Minnier, at <minnier@ohsu.edu>
## ==================================================================================== ##


## ==================================================================================== ##
## HEATMAP TAB
## ==================================================================================== ##
tabPanel(
    "Heatmaps",
    fluidRow(
        column(
            4, wellPanel(
                actionButton("action_heatmaps", "Generate Heatmaps"),
                h6(textOutput("numheat")),
                radioButtons("heatmapvaluename", label = "Select Value to Plot in Heatmap", choices = ""),
                checkboxGroupInput("view_group_heatmap",
                    label = h5("Select Groups to View"),
                    choices = "",
                    selected = ""
                ),
                radioButtons("heatmap_subset",
                    label = "Use all genes or upload your own subset?",
                    choices = c("all", "subset"), selected = "all"
                ),
                conditionalPanel(
                    "input.heatmap_subset=='subset'",
                    fileInput("heatmap_file", "Choose File Containing Gene IDs\n(one row per gene)",
                        accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"
                        )
                    )
                ),
                conditionalPanel(
                    "input.heatmap_subset=='all'",
                    radioButtons("heatmap_order",
                        label = "Order genes by",
                        choices = c(
                            "Significance (adjusted p-value)" = "significance",
                            "Variation (CV or SD)" = "variation"
                        ),
                        selected = "variation"
                    ),
                    h3("Filters"),
                    conditionalPanel(
                        condition = "input.heatmap_order=='significance'",
                        selectizeInput("sel_test_heatmap",
                            label = h5("Select Test to Use for Filtering"),
                            choices = "",
                            selected = ""
                        )
                    ),
                    conditionalPanel(
                        condition = "input.heatmap_order=='significance'",
                        checkboxInput("filter_fdr", "FDR cutoff", value = FALSE)
                    ),
                    conditionalPanel(
                        condition = "input.filter_fdr==true",
                        numericInput("FDRcut",
                            label = "Choose P-value
                                         (FDR if analyzed by START) cutoff",
                            min = 0, max = 1, value = 0.05
                        )
                    ),
                    conditionalPanel(
                        condition = "input.heatmap_order=='significance'",
                        checkboxInput("filter_fc", "Filter by fold change for a pair of groups",
                            value = FALSE
                        )
                    ),
                    conditionalPanel(
                        condition = "input.filter_fc==true",
                        selectizeInput("fold_change_groups",
                            label = "Select 2 Groups",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE, options = list(maxItems = 2)
                        ),
                        sliderInput("fold_change_range",
                            label = "Choose Log2Fold Change Filter",
                            min = -20, max = 20, value = c(-20, 20)
                        )
                    ),
                    checkboxInput("filter_maxgene",
                        "Show a maximum number of genes (WARNING: Selecting >5000 genes may be slow to load. If app crashes memory limits have been reached and you should run from local computer via github.)",
                        value = TRUE
                    ),
                    conditionalPanel(
                        condition = "input.filter_maxgene==true",
                        numericInput("maxgenes",
                            label = "Choose Max # of Genes",
                            min = 1, max = 10000, value = 100, step = 1
                        )
                    )

                    #                             #conditionalPanel(condition="output.numgenes>9000",
                    #                             conditionalPanel(condition="input.filter_maxgene==false",
                    #                                              radioButtons("filter_maxgeneN",
                    #                                                           "WARNING: >10000 genes may be slow to
                    # load and may cause memory limits to be reached and app may crash.
                    # Run app on local computer if heatmap of large number of genes and samples required.",
                    #                                                           choices=c(
                    #                                                             "Restrict to 10k genes for online viewing."="genesN",
                    #                                                             "Do not restrict to 10k. Show all genes (may cause web app to crash)."="genesall"),
                    #                                                           selected="genesN"
                    #                                              )
                    #                             )
                ), # conditional panel
                h3("Visualization Settings"),
                checkboxInput("heatmap_rowlabels",
                    "Show gene (row) labels",
                    value = TRUE
                ),
                checkboxInput("heatmap_rowcenter",
                    "Center each row",
                    value = TRUE
                ),
                checkboxInput("group_replicates",
                    "Group replicates by average",
                    value = FALSE
                ),
                checkboxInput("remove_geneids",
                    "Gene row label (gene names only)",
                    value = FALSE
                ),
                sliderInput("heatmap_width", "Heatmap image size (width)", min = 20, max = 100, value = 100),
                conditionalPanel(
                    "output.heatmapHighResAvailable",
                    downloadButton(
                        "downloadHighResHeatmap",
                        "Download Heatmap (High Res. pdf)"
                    )
                )
            ) # ,#sidebarPanel
            # img(src="KCardio_CMYK_4C_pos_small.jpg",height=150,width= 275,align="right")
        ), # column
        column(
            8,
            tabsetPanel(
                tabPanel(
                    title = "HeatMap",
                    # textOutput("which_genes"),
                    h4(textOutput("heatmap_rna_title")),
                    plotOutput("heatmap_rna", height = "1200px")
                ),
                # tabPanel(title="Interactive HeatMap",
                #          h4(textOutput("heatmap_rna_title_int")),
                #          uiOutput("heatmapggvisUI_rna"),
                #          ggvisOutput("heatmapggvis_rna")
                # ),
                tabPanel(
                    title = "Interactive HeatMap",
                    # h4(textOutput("heatmap_rna_title_int")),
                    plotlyOutput("heatmapplotly", height = "1200px")
                ),
                tabPanel(
                    title = "Data Output",
                    downloadButton(
                        "downloadHeatmapData_rna",
                        "Download Heatmap Data as CSV File"
                    ),
                    DT::dataTableOutput("heatdat_rna")
                )
            ) # tabsetPanel
        ) # column
    ) # fluidrow
) # tabpanel
