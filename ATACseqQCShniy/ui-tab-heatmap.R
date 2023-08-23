

tabItem(
    tabName = "heatmap_tab",
    fluidRow(

        conditionalPanel("output.multiple_bamfiles",

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
            wellPanel(
                h5("Assessing similarity of replicates"),
                withSpinner(plotOutput("plot_heatmap"))
            )
        )
        ),
        conditionalPanel("!output.multiple_bamfiles",
            h3('Upload multiple bamfiles for heatmap!!')
        )
    )
)
