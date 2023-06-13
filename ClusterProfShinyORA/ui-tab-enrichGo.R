tabItem(
    tabName = "enrichGoTab",
    conditionalPanel(
        "output.enrichGoAvailable",
        column(
            2,
            h3(strong("enrichGo Results")),
            hr(),
            checkboxInput("showGeneidGo", "Show geneID column", value = F),
            downloadButton("downloadEnrichGoCSV", "Save Results as CSV File", class = "btn btn-info", style = "margin: 7px;"),
            actionButton("gotoGoPlots", "enrichGo Plots", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoWordcloud", "Word Cloud", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;")
        ),
        column(
            10,
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("enrichGoTable")),
                tags$div(class = "clearBoth"),
            ),
            conditionalPanel(
                "input.enrichGoTable_rows_selected && input.enrichGoTable_rows_selected.length > 0",
                h3(strong("Selected entries")),
                tags$div(
                    class = "BoxArea2",
                    withSpinner(dataTableOutput("enrich_go_selected"))
                )
            ),
            tags$div(class = "clearBoth")
        ),
        tags$div(class = "clearBoth")
    )
)
