library(shiny)

ui <- fluidPage(
  selectInput("input_var", "Choose an option:", choices = c("Option 1", "Option 2")),
  
  conditionalPanel(
    condition = "output.output_value === 'Option 1'",
    p("This is displayed when Option 1 is selected.")
  ),
  
  conditionalPanel(
    condition = "output.output_value === 'Option 2'",
    p("This is displayed when Option 2 is selected.")
  )
)

server <- function(input, output) {
  output$output_value <- renderText({
    input$input_var
  })
}

shinyApp(ui, server)




