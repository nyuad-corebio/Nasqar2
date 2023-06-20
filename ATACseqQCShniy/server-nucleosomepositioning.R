


observeEvent(input$sel_sample_for_npositioning, {

    print('next_parameter_npos_qc')
    req(input$sel_sample_for_npositioning != '')

    # bamfile <- my_values$bamfile
    print('sel_sample_for_npositioning')
    bamfile <- paste0(my_values$base_dir, '/',my_values$samples_df[input$sel_sample_for_npositioning, 'BamFile'])
        # Create a BamFile object

        print(bamfile)
    bam <- BamFile(bamfile,yieldSize = 100)

    # Get the number of records in the BAM file
    record_count <-countBam(bam)$records

    # Print the number of records
    print(record_count)
    print(record_count)

    header <- scanBamHeader(bamfile)
    print('header')

    # Extract the chromosome names from the header

    print(header[[bamfile]]$targets)
    chromosomes <- names(header[[bamfile]]$targets)

    default_chromosome = NULL
    if ('chr1' %in% chromosomes){
        default_chromosome = 'chr1'
    }
    updateSelectInput(session, "sel_chromosome", choices = chromosomes, selected=default_chromosome)

    # Print the list of chromosomes
    print(chromosomes)


    updateNumericInput(session,"record_count_value", label=paste0('Records(max ',record_count, ')'), max = record_count )

 

})

inputDataReactive <- eventReactive(input$run_qc, {

    print('inputDataReactive')

    js$addStatusIcon("nucleosomepositioning_tab", "loading")


    tags_integer_types <- input$sel_tag_integer_type
    tags_char_types <- input$sel_tag_char_type
    bamfile <- paste0(my_values$base_dir, '/',my_values$samples_df[input$sel_sample_for_npositioning, 'BamFile'])

    # possibleTag <- list(
    #     "integer" = c(
    #         "AM", "AS", "CM", "CP", "FI", "H0", "H1", "H2",
    #         "HI", "IH", "MQ", "NH", "NM", "OP", "PQ", "SM",
    #         "TC", "UQ"
    #     ),
    #     "character" = c(
    #         "BC", "BQ", "BZ", "CB", "CC", "CO", "CQ", "CR",
    #         "CS", "CT", "CY", "E2", "FS", "LB", "MC", "MD",
    #         "MI", "OA", "OC", "OQ", "OX", "PG", "PT", "PU",
    #         "Q2", "QT", "QX", "R2", "RG", "RX", "SA", "TS",
    #         "U2"
    #     )
    # )
    possibleTag <- list(
         "integer" = tags_integer_types,
         "character" = tags_char_types

    )


    print(bamfile)
    bam <- scanBam(BamFile(bamfile, yieldSize = 10),
        param = ScanBamParam(tag = unlist(possibleTag))
    )


    # Create a BamFile object
    bam <- BamFile(bamfile,yieldSize = 100)

    # Get the number of records in the BAM file
    record_count <-countBam(bam)$records

    # Print the number of records
    print(record_count)
    print(record_count)
    updateNumericInput(session,"record_count_value", label=paste0('Records(max ',record_count, ')'), max = record_count )


    bamTop100 <- scanBam(BamFile(bamfile, yieldSize = input$record_count_value),
        param = ScanBamParam(tag = unlist(possibleTag))
    )[[1]]$tag
    print('start_bam')
    # print(bamTop100)
     print('end_bam')
    ntags <- names(bamTop100)[lengths(bamTop100) > 0]
    ntags
    print(ntags)
    
    


    ## shift the coordinates of 5'ends of alignments in the bam file
    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    ## if you don't have an available TxDb, please refer
    ## GenomicFeatures::makeTxDbFromGFF to create one from gff3 or gtf file.
    # seqlev <- "chr1" ## subsample data for quick run
    # seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)
    # which <- as(seqinformation[seqlev], "GRanges")
    # gal <- readBamFile(bamfile, tag = ntags, which = which, asMates = TRUE, bigFile = TRUE)
    # shiftedBamfile <- file.path(outPath, "shifted.bam")
    # gal1 <- shiftGAlignmentsList(gal, outbam = shiftedBamfile)
    # library(input$tx_db_input, character.only = T)
    # txs <- transcripts(get(input$tx_db_input))

    library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    ## if you don't have an available TxDb, please refer
    ## GenomicFeatures::makeTxDbFromGFF to create one from gff3 or gtf file.
    seqlev <- input$sel_chromosome ## subsample data for quick run
    seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)
    print(seqinformation)
    which <- as(seqinformation[seqlev], "GRanges")
    print(which)
    gal <- readBamFile(bamfile, tag = ntags, which = which, asMates = TRUE, bigFile = TRUE)
    
    outPath <- tempdir(check=TRUE)
    files <- list.files(outPath, full.names = T, recursive= TRUE)
    file.remove(files)

    outPath <- paste0(file.path(outPath, input$sel_sample_for_npositioning))
    dir.create(outPath)

    print('tempdir')
    print(dir(outPath))
    shiftedBamfile <- file.path(outPath, "shifted.bam")
    gal1 <- shiftGAlignmentsList(gal, outbam = shiftedBamfile)

    print('shiftGAlignmentsList')
    print(dir(outPath))

       
    library(input$tx_db_input, character.only = T)
    txs <- transcripts(get(input$tx_db_input))


    library(BSgenome.Hsapiens.UCSC.hg19)
    library(phastCons100way.UCSC.hg19)
    ## run program for chromosome 1 only
    txs1 <- txs[seqnames(txs) %in% input$sel_chromosome]
    genome <- Hsapiens
    ## split the reads into NucleosomeFree, mononucleosome,
    ## dinucleosome and trinucleosome.
    ## and save the binned alignments into bam files.
    objs <- splitGAlignmentsByCut(gal1,
        txs = txs1, genome = genome, outPath = outPath,
        conservation = phastCons100way.UCSC.hg19
    )




    my_values$outPath <- outPath
    return(list("gal1" = gal1, "txs"=txs))
})

output$plot_pt_score <- renderPlot({
    # files will be output into outPath
    req(input$run_qc)

    gal1 <- inputDataReactive()$gal1
    txs <- inputDataReactive()$txs
 
 

    pt <- PTscore(gal1, txs)
    plot(pt$log2meanCoverage, pt$PT_score,
        xlab = "log2 mean coverage",
        ylab = "Promoter vs Transcript"
    )
})


output$plot_nfr_score <- renderPlot({
    gal1 <- inputDataReactive()$gal1
    library(input$tx_db_input, character.only = T)
    txs <- transcripts(get(input$tx_db_input))
    nfr <- NFRscore(gal1, txs)
    plot(nfr$log2meanCoverage, nfr$NFR_score,
        xlab = "log2 mean coverage",
        ylab = "Nucleosome Free Regions score",
        main = "NFRscore for 200bp flanking TSSs",
        xlim = c(-10, 0), ylim = c(-5, 5)
    )
})


output$plot_tssre_score <- renderPlot({
    gal1 <- inputDataReactive()$gal1
    txs <- inputDataReactive()$txs
    tsse <- TSSEscore(gal1, txs)
    tsse$TSSEscore
    plot(100 * (-9:10 - .5), tsse$values,
        type = "b",
        xlab = "distance to TSS",
        ylab = "aggregate TSS score"
    )
})


output$plotCumulativePercentage <- renderPlot({

    gal1 <- inputDataReactive()$gal1
    txs <- inputDataReactive()$txs
    bamfile <- my_values$bamfile
    outPath <- my_values$outPath


    ## list the files generated by splitGAlignmentsByCut.
   

    # objs <- splitBam(bamfile, tag=ntags, outPath=outPath,
    #              txs=txs, genome=genome,
    #              conservation=phastCons100way.UCSC.hg19)
    print('outPath')
    print(dir(outPath))


    library(ChIPpeakAnno)



    seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)
    outPath <- my_values$outPath
    bamfiles <- file.path(
        outPath,
        c(
            "NucleosomeFree.bam",
            "mononucleosome.bam",
            "dinucleosome.bam",
            "trinucleosome.bam"
        )
    )
    # print(bamfiles)
    ## Plot the cumulative percentage of tag allocation in nucleosome-free
    ## and mononucleosome bam files.
    cumulativePercentage(bamfiles[1:2], as(seqinformation["chr1"], "GRanges"))
})


output$plot_featureAlignedHeatmap <- renderPlot({

    bamfiles <- file.path(outPath,
                     c("NucleosomeFree.bam",
                     "mononucleosome.bam",
                     "dinucleosome.bam",
                     "trinucleosome.bam"))

    seqlev <- input$sel_chromosome 
    gal1 <- inputDataReactive()$gal1
    txs <- inputDataReactive()$txs
    bamfile <- my_values$bamfile
    outPath <- my_values$outPath

    TSS <- promoters(txs, upstream=0, downstream=1)
    TSS <- unique(TSS)
    ## estimate the library size for normalization
    (librarySize <- estLibSize(bamfiles))
    NTILE <- 101
    dws <- ups <- 1010
    sigs <- enrichedFragments(
        gal = objs[c(
            "NucleosomeFree",
            "mononucleosome",
            "dinucleosome",
            "trinucleosome"
        )],
        TSS = TSS,
        librarySize = librarySize,
        seqlev = seqlev,
        TSS.filter = 0.5,
        n.tile = NTILE,
        upstream = ups,
        downstream = dws
    )
    ## log2 transformed signals
    sigs.log2 <- lapply(sigs, function(.ele) log2(.ele + 1))
    # plot heatmap

    output$plot_signals <- renderPlot({

        ## get signals normalized for nucleosome-free and nucleosome-bound regions.
        out <- featureAlignedDistribution(sigs, 
                                    reCenterPeaks(TSS, width=ups+dws),
                                    zeroAt=.5, n.tile=NTILE, type="l", 
                                    ylab="Averaged coverage")
        ## rescale the nucleosome-free and nucleosome signals to 0~1
        range01 <- function(x){(x-min(x))/(max(x)-min(x))}
        out <- apply(out, 2, range01)
        matplot(out, type="l", xaxt="n", 
                xlab="Position (bp)", 
                ylab="Fraction of signal")
        axis(1, at=seq(0, 100, by=10)+1, 
            labels=c("-1K", seq(-800, 800, by=200), "1K"), las=2)
        abline(v=seq(0, 100, by=10)+1, lty=2, col="gray")
    })

    featureAlignedHeatmap(sigs.log2, reCenterPeaks(TSS, width = ups + dws),
        zeroAt = .5, n.tile = NTILE
    )
})


output$plot_Footprints <- renderPlot({
    inputDataReactive()

    isolate({
        seqlev <- input$sel_chromosome 
    })
    
    outPath <- my_values$outPath
    shiftedBamfile <- file.path(outPath, "shifted.bam")
    library(MotifDb)
    CTCF <- query(MotifDb, c("CTCF"))
    CTCF <- as.list(CTCF)
    print(CTCF[[1]], digits=2)
    library(BSgenome.Hsapiens.UCSC.hg19)
    genome <- Hsapiens
    sigs <- factorFootprints(shiftedBamfile, pfm=CTCF[[1]], 
                         genome=genome, ## Don't have a genome? ask ?factorFootprints for help
                         min.score="90%", seqlev=seqlev,
                         upstream=100, downstream=100)
})

output$plot_featureAlignedHeatmap <- renderPlot({
    input$run_qc
    seqlev <- input$sel_chromosome 
    outPath <- my_values$outPath
    shiftedBamfile <- file.path(outPath, "shifted.bam")
    library(MotifDb)
    CTCF <- query(MotifDb, c("CTCF"))
    CTCF <- as.list(CTCF)
    print(CTCF[[1]], digits=2)
    library(BSgenome.Hsapiens.UCSC.hg19)
    genome <- Hsapiens

    inputDataReactive()
    js$addStatusIcon("nucleosomepositioning_tab", "done")
    sigs <- factorFootprints(shiftedBamfile, pfm=CTCF[[1]], 
                         genome=genome, ## Don't have a genome? ask ?factorFootprints for help
                         min.score="90%", seqlev=seqlev,
                         upstream=100, downstream=100)

    output$plot_segmentation <-  renderPlot({
        sigs$Profile.segmentation
    })

    output$plot_vp <- renderPlot({
        vp <- vPlot(shiftedBamfile, pfm=CTCF[[1]], 
            genome=genome, min.score="90%", seqlev=seqlev,
            upstream=200, downstream=200, 
            ylim=c(30, 250), bandwidth=c(2, 1))

         output$plot_distanceDyad <- renderPlot({
         distanceDyad(vp, pch=20, cex=.5)
         })
    })
   


    featureAlignedHeatmap(sigs$signal, 
                      feature.gr=reCenterPeaks(sigs$bindingSites,
                                               width=200+width(sigs$bindingSites[1])), 
                      annoMcols="score",
                      sortBy="score",
                      n.tile=ncol(sigs$signal[[1]]))


   
})



