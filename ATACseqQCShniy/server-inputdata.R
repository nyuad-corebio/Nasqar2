observe({
    shinyjs::hide(selector = "a[data-value=\"tab2\"]")
})


observe({
    # shinyjs::hide(selector = "a[data-value=\"tab2\"]")
    #ddsReactive()
})

my_values <- reactiveValues()

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


   

    print(input$data_file_type)

    if (input$data_file_type == "example_bam_file") {
        bamfile <- "www/exampleData/bamfiles/GL1.bam"


        bam_dir <- 'www/exampleData/bamfiles/'
        # Get the list of files in the directory
        files <- list.files(bam_dir, full.names = FALSE)

        # Filter BAM and BMI files
        bam_files <- files[grepl(".bam$", files)]
        bai_files <- files[grepl(".bai$", files)]

        print(bam_files)
        print(bai_files)

        # Check if there are matching BAM and BMI files
        matching_files <- sapply(bam_files, function(bam_file) {
            bmi_file <- gsub(".bam$", ".bai", bam_file)
            bmi_file %in% bai_files
        })

        sample_names <- sapply(bam_files, function(bam_file) {
            bam_file <- gsub(".bam$", "", bam_file)
        })
        sample_names <-  unname(sample_names)

        print(matching_files)

        samples_df <- data.frame(BamFile = bam_files, IndexFile = bai_files, row.names = sample_names )

      
        my_values$base_dir <-  'www/exampleData/bamfiles'
        my_values$samples_df <- samples_df

        output$bam_samples_table <-  renderTable({
            samples_df
        }, bordered = TRUE,
            spacing = "xs",
            rownames = T)

        print(samples_df)

# Print the matching files

        s <- sample_names


        print(s)
        print(bam_files)
        updateSelectInput(session, "sample_librarycomplexity", choices=s)
        updateSelectInput(session, "sample_fragmentsize", choices=s);
        updateSelectInput(session, "sel_sample_for_npositioning", choices=s, select=NULL);
        

    } else {
        bamfile <- input$bam_file_path
    }
        tags_integer_types <- c(
        "AM", "AS", "CM", "CP", "FI", "H0", "H1", "H2",
        "HI", "IH", "MQ", "NH", "NM", "OP", "PQ", "SM",
        "TC", "UQ"
    )

    tags_char_types <- c(
        "BC", "BQ", "BZ", "CB", "CC", "CO", "CQ", "CR",
        "CS", "CT", "CY", "E2", "FS", "LB", "MC", "MD",
        "MI", "OA", "OC", "OQ", "OX", "PG", "PT", "PU",
        "Q2", "QT", "QX", "R2", "RG", "RX", "SA", "TS",
        "U2"
    )
   
    updateSelectizeInput(session, "sel_tag_integer_type", choices = tags_integer_types, selected= tags_integer_types)
    updateSelectizeInput(session, "sel_tag_char_type", choices = tags_char_types, selected= tags_char_types)
    
    my_values$bamfile <- bamfile
    print('bamfile')
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
