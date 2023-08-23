library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(Rsamtools)
library(MotifDb)
library(ATACseqQC)
library(ChIPpeakAnno)
library(GenomicAlignments)
require(DT)

htmltags<- tags
ui <- dashboardPage(
    dashboardHeader(title = "ATACseqQC"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Input Data", tabName = "input_tab", icon = icon("upload")),
            menuItem("Heatmap", tabName = "heatmap_tab"),
            menuItem("Libraray Complexity", tabName = "librarycomplexity_tab"),
            menuItem("Fragment size", tabName = "fragmentsize_tab"),
            menuItem("Nucleosome Positioning", tabName = "nucleosomepositioning_tab")
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),

        htmltags$head(
            htmltags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
            htmltags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
            htmltags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        
        ),
        tabItems(
            source("ui-tab-inputdata.R", local = TRUE)$value,
            source("ui-tab-heatmap.R", local = TRUE)$value,
            source("ui-tab-librarycomplexity.R", local = TRUE)$value,
            source("ui-tab-fragmentsize.R", local = TRUE)$value,
            source("ui-tab-nucleosomepositioning.R", local = TRUE)$value
            
        )
    )
)
