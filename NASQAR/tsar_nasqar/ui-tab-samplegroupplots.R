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
##
##
## ==================================================================================== ##
## Group Plots
## ==================================================================================== ##
tabPanel(
    "Group Plots",
    fluidRow(
        column(
            4, wellPanel(
                selectizeInput("sampleres_groups",
                    label = "Select Groups",
                    choices = NULL,
                    multiple = TRUE
                ),
                selectizeInput("sampleres_samples",
                    label = "Select Samples",
                    choices = NULL,
                    multiple = TRUE
                ),
                conditionalPanel(
                    "input.groupplot_tabset=='PCA Plot'",
                    selectizeInput("pcnum",
                        label = "Select Principal Components",
                        choices = 1:10,
                        multiple = TRUE,
                        selected = 1:2,
                        options = list(maxItems = 2)
                    )
                )
            ) # ,#wellpanel


            # img(src="KCardio_CMYK_4C_pos_small.jpg",height=150,width= 275,align="right")
        ), # column
        column(
            8,
            tabsetPanel(
                id = "groupplot_tabset",
                tabPanel(
                    title = "PCA Plot",
                    plotOutput("pca_plot", height = "600px")
                ), # tabPanel
                tabPanel(
                    title = "Sample Distance Heatmap",
                    plotOutput("gene_pheatmap")
                ), # tabPanel
                tabPanel(
                    title = "Sample Distance Heatmap - Deseq",
                    plotOutput("gene_pheatmap_deseq")
                ) # tabPanel
            ) # tabsetPanel
        ) # mainPanel
    ) # sidebarLayout
) # END tabPanel
