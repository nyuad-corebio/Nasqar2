

tabItem(
    tabName = "filter_and_trim_tab",
    fluidRow(
        column(4,
              actionButton("runQc", "RunQc")
             
        ),
        column(12,
               withSpinner(dataTableOutput('filterAndTrim_output_table'))
          
        )
    )
)
