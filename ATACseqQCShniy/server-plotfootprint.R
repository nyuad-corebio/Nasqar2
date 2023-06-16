observe({
    shinyjs::hide(selector = "a[data-value=\"tab2\"]")
})


observe({
    shinyjs::hide(selector = "a[data-value=\"tab2\"]")
    #ddsReactive()
})



observeEvent(input$bs_genome_input, {
    if(input$bs_genome_input != 'empty'){
        tx_db <- list("BSgenome.Hsapiens.UCSC.hg19" = c("TxDb.Hsapiens.UCSC.hg19.knownGene"))

        updateSelectInput(session, "tx_db_input", choices = tx_db[[input$bs_genome_input]])
    }

})

observeEvent(input$tx_db_input, {
    if(input$bs_genome_input != 'empty'){
        phast_cons <- list("BSgenome.Hsapiens.UCSC.hg19" = c("phastCons100way.UCSC.hg19"))

        updateSelectInput(session, "phast_cons_input", choices = phast_cons[[input$bs_genome_input]])
    }

})



observe({
    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need(identical(input$data_file_type, "example_bam_file") | (!is.null(input$bam_file_path)),
            message = "Please select a file"
        )
    )

    bs_genome_db <- c( " "="empty", "Human (BSgenome.Hsapiens.UCSC.hg19)" = "BSgenome.Hsapiens.UCSC.hg19")
    
    updateSelectInput(session, "bs_genome_input", choices = bs_genome_db)


   



    if (input$data_file_type == "upload_bam_file") {
        bamfile <- "www/exampleData/drosphila_example_de.csv"
    } else {
        bamfile <- input$bam_file_path
    }
    print(bamfile)
    js$collapse("uploadbox")

    # output$plot <- renderPlot({
    #     js$addStatusIcon("tab1", "loading")
    #     withProgress(
    #         message = "Calculation in progress",
    #         detail = "This may take a while...",
    #         value = 0,
    #         {
    #             for (i in 1:15) {
    #                 incProgress(1 / 15)
    #                 Sys.sleep(0.25)
    #             }
    #         }
    #     )
    #     plot(cars)
    #     js$addStatusIcon("tab1", "done")
    #     shinyjs::show(selector = "a[data-value=\"tab2\"]")
    # })
})
