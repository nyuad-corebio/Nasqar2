observe({
   
    shinyjs::hide(selector = "a[data-value=\"fragmentsize_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"nucleosomepositioning_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"heatmap_tab\"]")
    shinyjs::hide(selector = "a[data-value=\"librarycomplexity_tab\"]")
    
})


observe({
    # shinyjs::hide(selector = "a[data-value=\"tab2\"]")
    #ddsReactive()
})



my_values <- reactiveValues()
my_values$mounted_dir <- FALSE

observeEvent(input$connect_remote_server, {

  
  print(input$username)
  print(input$hostname)
  print(input$mountpoint)
  print(input$id_rsa$datapath)
  my_values$mounted_dir <- FALSE
  
  
  system(paste("sh generate_ssh_config.sh ", input$username, " ", 
    input$hostname, " ", input$mountpoint, " ",
    input$id_rsa$datapath))
  

        # Get the list of files in the directory
    # files <- list.files(base_dir, full.names = FALSE)
    my_values$mounted_dir <- TRUE
    
  
})




observeEvent(input$bs_genome_input, {
    print(input$bs_genome_input)
    if(input$bs_genome_input != "" & input$bs_genome_input != " "){
        print('input$bs_genome_input')
        print(input$bs_genome_input)
        library(input$bs_genome_input, character.only = T)
        tx_db <- list("BSgenome.Hsapiens.UCSC.hg19" = c("TxDb.Hsapiens.UCSC.hg19.knownGene"))
         shinyjs::enable("initQC")
        # updateSelectInput(session, "tx_db_input", choices = tx_db[[input$bs_genome_input]])
    }

})

# observeEvent(input$tx_db_input, {
#     if(input$bs_genome_input != 'empty'){
#         phast_cons <- list("BSgenome.Hsapiens.UCSC.hg19" = c("phastCons100way.UCSC.hg19"))

#         updateSelectInput(session, "phast_cons_input", choices = phast_cons[[input$bs_genome_input]])
       
#     }

# })

input_files_reactive <- reactive({
    
    # shiny::validate(
    #     need(identical(input$data_file_type, "example_bam_file") | (is.null(input$bam_files)),
    #         message = "Please select a file"
    #     )
    # )

    shiny::validate(
        need(identical(input$data_file_type, "example_fastq_file")|  identical(input$data_file_type, "mount_remote_server") | (identical(input$data_file_type, "upload_fastq_file") &  !is.null(input$fastq_files) & length(input$fastq_files$name) > 1),
            message = "Please upload both R1 andd R2 fastq files "
        )
    )

   
     


    shiny::validate(
        need(identical(input$data_file_type, "example_fastq_file") | identical(input$data_file_type, "upload_fastq_file")|  identical(input$data_file_type, "mount_remote_server") & my_values$mounted_dir ,
            message = "Please connect to the server "
        )
    )
 

    

    if(identical(input$data_file_type, "upload_fastq_file")){
       print(input$fastq_files)
        files <- c(input$fastq_files$name)
  
        # Filter fastq files
        # Filter fastq files
        fn_Fs <- files[grepl("_R1_001.fastq", files)]
        fn_Rs <- files[grepl("_R2_001.fastq", files)]

        matching_files <- sapply(fn_Fs, function(fastq_file) {
          fastq_file <- gsub("_R1_001.fastq$", "_R2_001.fastq", fastq_file)
          fastq_file %in% fn_Rs
        })
        fn_Fs <- names(matching_files[matching_files == TRUE])
        fn_Rs <- stringr::str_replace_all(fn_Fs, "_R1_001.fastq", "_R2_001.fastq" )

        shiny::validate(
            need(sum(matching_files) == length(fn_Fs) & sum(matching_files)  == length(fn_Rs),
                message = "Please upload both R1 and R2 fastq files having same base name(ex: <name>_R1_001.fastq and <name>_R2_001.fastq) "
            )
        )

        base_dir <- tempfile()
        dir.create(base_dir)
        apply(input$fastq_files, 1, function(row) {
            # Access row elements by index or name
            # print(str(row))
            datapath <- row["datapath"]
            name <- row["name"]
            file.remove(file.path(base_dir, name))
            file.symlink(datapath, file.path(base_dir, name))
            # file.copy(datapath, file.path(base_dir, name))
            # print(name)
        })
       
    } else  if(identical(input$data_file_type, "example_fastq_file")){
       
        base_dir <- 'www/exampleData/MiSeq_SOP/'
        # Get the list of files in the directory
        files <- list.files(base_dir, full.names = FALSE)

        # Filter fastq files
        fn_Fs <- files[grepl("_R1_001.fastq", files)]
        fn_Rs <- files[grepl("_R2_001.fastq", files)]
        matching_files <- sapply(fn_Fs, function(fastq_file) {
          fastq_file <- gsub("_R1_001.fastq$", "_R2_001.fastq", fastq_file)
          fastq_file %in% fn_Rs
        })
        fn_Fs <- names(matching_files[matching_files == TRUE])
        fn_Rs <- stringr::str_replace_all(fn_Fs, "_R1_001.fastq", "_R2_001.fastq" )
        
    }else {
        base_dir <- './mnt'
        # Get the list of files in the directory
        files <- list.files(base_dir, full.names = FALSE)

              # Filter fastq files
        fn_Fs <- files[grepl("_R1_001.fastq", files)]
        fn_Rs <- files[grepl("_R2_001.fastq", files)]
        matching_files <- sapply(fn_Fs, function(fastq_file) {
          fastq_file <- gsub("_R1_001.fastq$", "_R2_001.fastq", fastq_file)
          fastq_file %in% fn_Rs
        })
        fn_Fs <- names(matching_files[matching_files == TRUE])
        fn_Rs <- stringr::str_replace_all(fn_Fs, "_R1_001.fastq", "_R2_001.fastq" )
        shiny::validate(
            need(sum(matching_files) == length(fn_Fs) & sum(matching_files)  == length(fn_Rs),
                message = "Please upload both R1 and R2 fastq files having same base name(ex: <name>_R1_001.fastq and <name>_R2_001.fastq) "
            )
        )
    }

            
    sample_names <- sapply(fn_Fs, function(fastq_file) {
      fastq_file <- gsub("_R1_001.fastq$", "", fastq_file)
    })

    sample_names <-  unname(sample_names)

    print(dir(base_dir))
    

    samples_df <- data.frame(FASTQ_Fs = fn_Fs, FASTQ_Rs = fn_Rs, row.names = sample_names )
    my_values$base_dir <-  base_dir
    my_values$samples_df <- samples_df
    
    updateSelectInput(session, "sel_sample_qualityprofile_tab", choices = sample_names,selected = NULL)
    # library(input$bs_genome_input, character.only = T)
    # bamfile <- my_values$bamfile
    print('sel_sample_for_npositioning')
    
    samples_df
})


output$fastqfiles_uploaded <- reactive({
  return(!is.null(input_files_reactive()))
})
outputOptions(output, 'fastqfiles_uploaded', suspendWhenHidden=FALSE)


output$fastq_samples_table <-  DT::renderDataTable({
            input_files_reactive()
},options = list(scrollX = TRUE, pageLength = 15))


