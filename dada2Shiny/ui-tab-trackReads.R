

tabItem(
    tabName = "trackReadsTab",
    fluidRow(
        # column(12,
        # column(
        #     4,wellPanel(
        #     selectInput("selSample4trackReadsTab", "Salect Sample", choices = NULL, selected = NULL))
        # ),
        # ),
        column(12,
        column(
            12,
            withSpinner(dataTableOutput('trackTable'))
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
        
        )
    )
)
