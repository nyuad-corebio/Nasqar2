

tabItem(
    tabName = "qualityprofile_tab",
    fluidRow(
        column(12,
        column(
            4,wellPanel(
            selectInput("sel_sample_qualityprofile_tab", "Salect Sample", choices = NULL, selected = NULL))
        ),
        ),
        column(12,
        column(
            6,
            withSpinner(plotOutput("plot_qualityprofile_fs"))
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        ),
        column(
          6,
          withSpinner(plotOutput("plot_qualityprofile_rs"))
          
          # actionButton("run_deseq2", "Run DESeq2",
          #              class = "btn btn-success",
          #              style = "width:100%;height:60px;"
          # ),
          # plotOutput("plot")
        )
        )
    )
)
