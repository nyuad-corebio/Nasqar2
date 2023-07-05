options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output,session) {

  source("server-inputdata.R", local = TRUE)
  source("server-heatmap.R", local = TRUE)
  source("server-librarycomplexity.R", local = TRUE)
  source("server-fragmentsize.R", local = TRUE)
  source("server-nucleosomepositioning.R", local = TRUE)
}