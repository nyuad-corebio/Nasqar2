

tabItem(
    tabName = "fragmentsize_tab",
    fluidRow(
        column(
            7,
            plotOutput("plot_fragmentsize")
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
