

tabItem(
    tabName = "errorRatesTab",
    fluidRow(
        column(12,
       
        ),
        column(12,
        column(
            6,
            withSpinner(plotOutput("plotErrors_errF"))
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        ),
        column(
          6,
          withSpinner(plotOutput("plotErrors_errR"))
          
          # actionButton("run_deseq2", "Run DESeq2",
          #              class = "btn btn-success",
          #              style = "width:100%;height:60px;"
          # ),
          # plotOutput("plot")
        )
        )
    )
)
