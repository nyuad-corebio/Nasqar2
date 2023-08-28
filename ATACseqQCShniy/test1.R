library(shiny)
library(glue)
library(tidyverse)

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Test Multi-File Download"),
  p("I hope this works!"),
  downloadButton(
    outputId = "download_btn",
    label = "Download",
    icon = icon("file-download")
  )
)

# Define server logic 
server <- function(input, output) {
  
  #datasets stored in reactiveValues list
  to_download <- reactiveValues(dataset1 = iris, dataset2 = airquality, dataset3 = mtcars, dataset4 = NULL)
  blahblah <- iris
  
  output$download_btn <- downloadHandler(
    filename = function(){
      paste("my_data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      reactiveValuesToList(to_download) %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name <- glue("{y}_data.csv")
            readr::write_csv(x, file.path(temp_directory, file_name))
          }
        })
      
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
      
      
    },
    contentType = "application/zip"
    
  )
  
  
}

shinyApp(ui = ui, server = server)