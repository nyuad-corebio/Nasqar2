require(shinydashboard)
require(shinyjs)
require(shinyBS)
require(shinycssloaders)
require(DT)
require(shiny)
library(rhandsontable)
library(readr)
library(RColorBrewer)
library(DESeq2)
library(heatmaply)
library(ggplot2)
library(ggthemes)
library(plotly)
library(BiocParallel)
library(sodium)
library(NMF)
library(tidyr)
library(dplyr)
library(sva)
library(uuid)


# library(pheatmap)
library(VennDiagram)
library(smplot2)
library(EnhancedVolcano)
library(rclipboard)

library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(GetoptLong)

# library(rhandsontable)
# library(readr)
# library(RColorBrewer)
# library(DESeq2)
# library(heatmaply)
# library(ggplot2)
# library(ggthemes)


# Venndiagram page
library(gridExtra)






appCSS <- "
#loading-content {
position: absolute;
background: #3c79ad;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

ui <- tagList(



    # The main app code goes here
    # hidden(
    div(
        id = "app-content",
        # class = "hidden",
        dashboardPage(
            # skin = "purple",
            dashboardHeader(title = "DESeq2 Shiny"),
            dashboardSidebar(
                sidebarMenu(
                    id = "tabs",
                    menuItem("0. User Guide", tabName = "introTab", icon = icon("info-circle")),
                    menuItem("1. Input Data", tabName = "inputdata", icon = icon("upload")),
                    menuItem("2. Design Conditions", tabName = "conditionsTab"),
                    menuItem("3a. Hidden Batch Effect", tabName = "svaseqTab"),
                    menuItem("3. Run DESeq2", tabName = "deseqTab"),
                    menuItem("   Rlog", tabName = "rlogTab", icon = icon("bar-chart")),
                    menuItem("   VST", tabName = "vstTab", icon = icon("bar-chart")),
                    menuItem("   DE Results", tabName = "resultsTab", icon = icon("chart-column")),
                    menuItem("   Vocano Plot", tabName = "volcanoplotTab"),
                    menuItem("   Venn Digram", tabName = "venndiagramTab"),
                    menuItem("   Gene Boxplot", tabName = "boxplotTab", icon = icon("bar-chart")),
                    menuItem("   Heatmap", tabName = "heatmapTab", icon = icon("bar-chart"))
                )
            ),
            dashboardBody(
                shinyjs::useShinyjs(),
                # inlineCSS(appCSS),
                extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),
                #
                tags$head(
                    tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
                    tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                    # ,
                    # tags$link(rel = "stylesheet", type = "text/css", href = "loading.css")
                ),
                tabItems(
                    source("ui-tab-intro.R", local = TRUE)$value,
                    source("ui-tab-inputdata.R", local = TRUE)$value,
                    source("ui-tab-conditions.R", local = TRUE)$value,
                    source("ui-tab-svaseq.R", local = TRUE)$value,
                    source("ui-tab-deseq.R", local = TRUE)$value,
                    source("ui-tab-rlog.R", local = TRUE)$value,
                    source("ui-tab-vst.R", local = TRUE)$value,
                    source("ui-tab-analysisres.R", local = TRUE)$value,
                    source("ui-tab-volcanoplot.R", local = TRUE)$value,
                    source("ui-tab-venndiagram.R", local = TRUE)$value,
                    source("ui-tab-boxplot.R", local = TRUE)$value,
                    source("ui-tab-heatmap.R", local = TRUE)$value
                )
            )
        ),
        tags$footer(
            wellPanel(
                HTML('
               <p align="center" width="4">Core Bioinformatics, Center for Genomics and Systems Biology, NYU Abu Dhabi</p>
      <p align="center" width="4">Github: <a href="https://github.com/nyuad-corebio/Nasqar2/tree/main//deseq2shiny/">https://github.com/nyuad-corebio/Nasqar2/tree/main//deseq2shiny/</a></p>
               <p align="center" width="4">Maintained by: <a href="mailto:nr83@nyu.edu">Nabil Rahiman</a> </p>
                <p align="center" width="4">Issues / Features requests: <a href="https://github.com/nyuad-corebio/Nasqar2/tree/main//deseq2shiny/issues" target="_blank">https://github.com/nyuad-corebio/Nasqar2/tree/main//deseq2shiny/issues</a></p>
               <p align="center" width="4">Using <a href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html" target="_blank">DESeq2</a> version 1.24.0</p>
               <p align="center" width="4">Copyright (C) 2019, code licensed under GPLv3</p>')
            ),
            tags$script(src = "imgModal.js")
        )
    )
    #   ,
    #   # Loading message
    #   div(
    #     id = "loading-content",
    #
    #     HTML('<div class="row vertical-center">
    # <h2>Loading application, please wait ...
    # <div class="spinner">
    #   <div class="double-bounce1"></div>
    #          <div class="double-bounce2"></div>
    #          </div>
    #          </h2>
    #          </div>')
    #   )
)
