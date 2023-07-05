output$plot_heatmap <-  renderPlot({

    

    # path <- system.file("extdata", package="ATACseqQC", mustWork=TRUE)
    path <- my_values$base_dir
    bamfiles <- dir(path, "*.bam$", full.name=TRUE)
    
    gals <- lapply(bamfiles, function(bamfile){
                readBamFile(bamFile=bamfile, tag=character(0), 
                            which=GRanges(input$sel_chromosome_heatmap_tab, IRanges(1, 1e6)), 
                            asMates=FALSE)
            })
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    txs <- transcripts(TxDb.Hsapiens.UCSC.hg19.knownGene)
    library(GenomicAlignments)
    plotCorrelation(GAlignmentsList(gals), txs, seqlev="chr1")

})

