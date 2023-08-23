

tabItem(
    tabName = "librarycomplexity_tab",
    fluidRow(
        column(
            4,wellPanel(
            selectInput("sample_librarycomplexity", "Salect Sample File", choices = NULL, selected = NULL))
        ),
        column(
            7,
            wellPanel(
            h5("Assessing sequencing depth and library complexity"),
            withSpinner(plotOutput("plot_libcomplexity")),
            h5("BAM files without removing duplicates are expected for estimating library complexity!")
            )
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
