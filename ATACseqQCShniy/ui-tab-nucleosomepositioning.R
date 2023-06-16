

tabItem(
    tabName = "nucleosomepositioning_tab",
    fluidRow(
        column(
            7,
            plotOutput("plot_pt_score"),
            # plotOutput("plotCumulativePercentage"),
            plotOutput("plot_tssre_score"),
            plotOutput("plotCumulativePercentage"),
            plotOutput("plot_featureAlignedHeatmap"),
            plotOutput('plot_signals'),
            plotOutput('plot_Footprints'),
            plotOutput('plot_segmentation'),
            plotOutput('plot_vp'),
            plotOutput('plot_distanceDyad')

      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)

