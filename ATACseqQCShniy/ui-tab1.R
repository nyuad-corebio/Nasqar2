
tabItem(
  tabName = "input_tab",
  fluidRow(
    column(
      12, h1('input_tab'),
      actionButton("run_deseq2", "Run DESeq2",
                   class = "btn btn-success",
                   style = "width:100%;height:60px;"
      ),
      plotOutput("plot")
    )
  )
)

