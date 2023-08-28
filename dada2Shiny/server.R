options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output,session) {

  source("server-inputdata.R", local = TRUE)
  source("server-qualityprofile.R", local = TRUE)
  source("server-filter_and_trim.R", local = TRUE)
  source("server-errorRates.R", local = TRUE)
  source("server-mergePairedReads.R", local = TRUE)
  source("server-trackReads.R", local = TRUE)
 
}