

tabItem(
    tabName = "heatmap_tab",
    fluidRow(

        column(
            4,wellPanel(
            selectInput("sel_chromosome_heatmap_tab",
                            label = "Chromosome", # or Ensembl ID",
                            choices = NULL,
                         )
            )
        ),
        
        column(
            7,
            h3('Correlations for multiple samples'),
            plotOutput("plot_heatmap")
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
