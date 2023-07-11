

tabItem(
    tabName = "fragmentsize_tab",
    fluidRow(
        column(
            4,wellPanel(
            selectInput("sample_fragmentsize", "Select Sample File", choices = NULL, selected = NULL))
        ),
        column(
            7,
            withSpinner(plotOutput("plot_fragmentsize"))
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
