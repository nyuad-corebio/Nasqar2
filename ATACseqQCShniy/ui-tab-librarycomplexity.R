

tabItem(
    tabName = "librarycomplexity_tab",
    fluidRow(
        column(
            7,
            plotOutput("plot_libcomplexity")
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
