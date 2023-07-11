
output$multiple_bamfiles <- reactive({
 
    req(my_values$base_dir)

    path <- my_values$base_dir
    bamfiles <- dir(path, "*.bam$", full.name=TRUE)

    if(length(bamfiles) > 1){
        return(TRUE)
    } else {
         return(FALSE)
    }

    
})
outputOptions(output, "multiple_bamfiles", suspendWhenHidden = FALSE)

output$plot_heatmap <-  renderPlot({



    # path <- system.file("extdata", package="ATACseqQC", mustWork=TRUE)
    path <- my_values$base_dir
    bamfiles <- dir(path, "*.bam$", full.name=TRUE)

    
    
    gals <- lapply(bamfiles, function(bamfile){
                param <- ScanBamParam(tag=character(0), which=GRanges(input$sel_chromosome_heatmap_tab, IRanges(1, 1e6)))
                # bfile <- readBamFile(bamFile=bamfile, tag=character(0), 
                #             which=GRanges(input$sel_chromosome_heatmap_tab, IRanges(1, 1e6)), 
                #             asMates=TRUE, bigFile=TRUE)

                readGAlignments(bamfile, use.names=TRUE, param=param)
            })
    library(input$bs_genome_input, character.only = T)
    # library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    tx_db <- tx_db_list[[input$bs_genome_input]]
    print(tx_db)
    library(tx_db, character.only = T)
    print(get(tx_db))
    txs <- transcripts(get(tx_db))
  
    plotCorrelation(GAlignmentsList(gals), txs, seqlev=input$sel_chromosome_heatmap_tab)

})

