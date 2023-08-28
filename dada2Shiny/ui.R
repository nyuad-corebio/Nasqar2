library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
require(DT)
library(dada2)
library(dplyr)
htmltags<- tags
ui <- dashboardPage(
    dashboardHeader(title = "DADA2"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Input Data", tabName = "input_tab", icon = icon("upload")),
            menuItem("Quality Profile", tabName = "qualityprofile_tab"),
            menuItem("Filter&Trime", tabName = "filter_and_trim_tab"),
            menuItem("Error Rate", tabName = "errorRatesTab"),
            menuItem("Merged Paired Reads", tabName = "margePairedReadsTab"),
            menuItem("Track Reads", tabName = "trackReadsTab")
           
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
            source("ui-tab-qualityprofile.R", local = TRUE)$value,
            source("ui-tab-filter_and_trim.R", local = TRUE)$value,
            source("ui-tab-errorRates.R", local = TRUE)$value,
            source("ui-tab-mergePairedReads.R", local = TRUE)$value,
            source("ui-tab-trackReads.R", local = TRUE)$value
          
            
        )
    )
)
