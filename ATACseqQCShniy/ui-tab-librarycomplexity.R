

tabItem(
    tabName = "librarycomplexity_tab",
    fluidRow(
        column(
            4,wellPanel(
            selectInput("sample_librarycomplexity", "Salect Sample File", choices = NULL, selected = NULL))
        ),
        column(
            7,
            withSpinner(plotOutput("plot_libcomplexity"))
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
