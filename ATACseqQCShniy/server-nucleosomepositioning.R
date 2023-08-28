tx_db_list <- list(
    "BSgenome.Hsapiens.UCSC.hg19" = c("TxDb.Hsapiens.UCSC.hg19.knownGene"),
    "BSgenome.Mmusculus.UCSC.mm10" = c("TxDb.Mmusculus.UCSC.mm10.knownGene"),
    "BSgenome.Drerio.UCSC.danRer11" = c("TxDb.Drerio.UCSC.danRer11.refGene"),
    "BSgenome.Celegans.UCSC.ce11" = c("TxDb.Celegans.UCSC.ce11.refGene")
)
phast_cons_list <- list("BSgenome.Hsapiens.UCSC.hg19" = c("phastCons100way.UCSC.hg19"))



bamfile_path <- reactive({
    if (input$data_file_type == "example_bam_file") {
        bamfile <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_for_npositioning, "BamFile"])
    } else {
        bafiles <- input$bam_files
    }
})

observeEvent(input$sel_sample_for_npositioning, {
    # output$plot_vp
    # output$plot_distanceDyad
    # output$plot_featureAlignedHeatmap
    # output$plot_nfr_score
    # output$plot_Footprints
    # output$plot_vp
    # output$p
})

my_values$done <- FALSE
output$task_done <- reactive({
    if (my_values$done) {
        return(TRUE)
    } else {
        return(FALSE)
    }
})

outputOptions(output, "task_done", suspendWhenHidden = FALSE)


inputDataReactive <- eventReactive(input$run_qc, {
    print("inputDataReactive")
    isolate({
        my_values$done <- FALSE
    })


    js$addStatusIcon("nucleosomepositioning_tab", "loading")

    library(tx_db_list[[input$bs_genome_input]], character.only = T)


    tags_integer_types <- input$sel_tag_integer_type
    tags_char_types <- input$sel_tag_char_type
    bamfile <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_for_npositioning, "BamFile"])

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
    # possibleTag <- list(
    #      "integer" = tags_integer_types,
    #      "character" = tags_char_types

    # )


    # print(bamfile)
    # bam <- scanBam(BamFile(bamfile, yieldSize = 100),
    #     param = ScanBamParam(tag = unlist(possibleTag))
    # )


    # Create a BamFile object
    # bam <- BamFile(bamfile,yieldSize = 100)

    # Get the number of records in the BAM file
    # record_count <-countBam(bam)$records

    # Print the number of records
    # print(record_count)
    # print(record_count)
    # updateNumericInput(session,"record_count_value", label=paste0('Records(max ',record_count, ')'), max = record_count )

    ## bamfile tags to be read in
    possibleTag <- combn(LETTERS, 2)
    possibleTag <- c(
        paste0(possibleTag[1, ], possibleTag[2, ]),
        paste0(possibleTag[2, ], possibleTag[1, ])
    )
    bamTop100 <- scanBam(BamFile(bamfile, yieldSize = 100),
        param = ScanBamParam(tag = possibleTag)
    )[[1]]$tag
    ntags <- names(bamTop100)[lengths(bamTop100) > 0]

    print(ntags)

    # library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    ## if you don't have an available TxDb, please refer
    ## GenomicFeatures::makeTxDbFromGFF to create one from gff3 or gtf file.
    seqlev <- input$sel_chromosome ## subsample data for quick run
    txb <- tx_db_list[[input$bs_genome_input]]
    seqinformation <- seqinfo(get(txb))
    as(seqinformation[input$sel_chromosome], "GRanges")
    # seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)
    print(seqinformation)

    which <- as(seqinformation[seqlev], "GRanges")
    my_values$GRanges <- which
    gal <- readBamFile(bamfile, tag = ntags, which = which, asMates = TRUE, bigFile = TRUE)

    outPath <- tempdir()
    outPath <- paste0(file.path(outPath, input$sel_sample_for_npositioning))
    dir.create(outPath)

    files <- list.files(outPath, full.names = T, recursive = TRUE)
    file.remove(files)

    shiftedBamfile <- file.path(outPath, "shifted.bam")
    gal1 <- shiftGAlignmentsList(gal, outbam = shiftedBamfile)

    print(dir(outPath))


    # Promoter/Transcript body (PT) score
    # PT score is calculated as the coverage of promoter divided by the coverage of its transcript body.
    # PT score will show if the signal is enriched in promoters.

    tx_db <- tx_db_list[[input$bs_genome_input]]
    library(tx_db, character.only = T)
    txs <- transcripts(get(tx_db))
    my_values$pt <- PTscore(gal1, txs)
    # plot will be called under renderPlot
    # plot(pt$log2meanCoverage, pt$PT_score,
    #  xlab="log2 mean coverage",
    #  ylab="Promoter vs Transcript")


    # Nucleosome Free Regions (NFR) score
    my_values$nfr <- NFRscore(gal1, txs)
    # plot will be called under renderPlot
    # plot(nfr$log2meanCoverage, nfr$NFR_score,
    #  xlab="log2 mean coverage",
    #  ylab="Nucleosome Free Regions score",
    #  main="NFRscore for 200bp flanking TSSs",
    #  xlim=c(-10, 0), ylim=c(-5, 5))

    ####################################################################################################
    # Transcription Start Site (TSS) Enrichment Score
    # TSS enrichment score is a raio between aggregate distribution of reads centered on TSSs and that
    # flanking the corresponding TSSs. TSS score = the depth of
    # TSS (each 100bp window within 1000 bp each side) / the depth of end flanks (100bp each end).
    # TSSE score = max(mean(TSS score in each window)). TSS enrichment score is calculated according
    # to the definition at https://www.encodeproject.org/data-standards/terms/#enrichment.
    # Transcription start site (TSS) enrichment values are dependent on the reference files used;
    # cutoff values for high quality data are listed in the following table from https://www.encodeproject.org/atac-seq/.
    ####################################################################################################

    my_values$tsse <- TSSEscore(gal1, txs)
    # plot will be called under renderPlot
    # plot(100*(-9:10-.5), tsse$values, type="b",
    #  xlab="distance to TSS",
    #  ylab="aggregate TSS score")

    # =======================================================
    # Split reads
    # =======================================================

    txs1 <- txs[seqnames(txs) %in% input$sel_chromosome]
    genome <- get(unlist(strsplit(input$bs_genome_input, "\\."))[[2]])
    ## split the reads into NucleosomeFree, mononucleosome,
    ## dinucleosome and trinucleosome.
    ## and save the binned alignments into bam files.
    objs <- splitGAlignmentsByCut(gal1,
        txs = txs1, genome = genome, outPath = outPath
        # conservation = phastCons100way.UCSC.hg19
    )


    ####################################################################################################
    # Heatmap and coverage curve for nucleosome positions
    # By averaging the signal across all active TSSs, we should observe that nucleosome-free
    # fragments are enriched at the TSSs, whereas the nucleosome-bound fragments should be
    # enriched both upstream and downstream of the active TSSs and display characteristic phasing
    # of upstream and downstream nucleosomes. Because ATAC-seq reads are concentrated at regions
    # of open chromatin, users should see a strong nucleosome signal at the +1 nucleosome, but
    # the signal decreases at the +2, +3 and +4 nucleosomes.
    ####################################################################################################

    my_values$outPath <- outPath
    bamfile <- my_values$bamfile
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

    TSS <- promoters(txs, upstream = 0, downstream = 1)
    TSS <- unique(TSS)
    ## estimate the library size for normalization
    (librarySize <- estLibSize(bamfiles))
    ## splited/NucleosomeFree.bam splited/mononucleosome.bam
    ##                      34276                       1933
    ##   splited/dinucleosome.bam  splited/trinucleosome.bam
    ##                       1871                        403

    NTILE <- 101 # Not sure chosen value?
    dws <- ups <- 1010 # Not sure chosen value?
    tss_sigs <- enrichedFragments(
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
    tss_sigs.log2 <- lapply(tss_sigs, function(.ele) log2(.ele + 1))
    tss_center_peaks <- reCenterPeaks(TSS, width = ups + dws)
    my_values$tss_center_peaks <- tss_center_peaks


    ## get signals normalized for nucleosome-free and nucleosome-bound regions.
    out <- featureAlignedDistribution(tss_sigs,
        tss_center_peaks,
        zeroAt = .5, n.tile = NTILE, type = "l",
        ylab = "Averaged coverage"
    )

    range01 <- function(x) {
        (x - min(x)) / (max(x) - min(x))
    }
    out <- apply(out, 2, range01)
    my_values$out <- out

    ####################################################################################################
    # plot Footprints
    # ATAC-seq footprints infer factor occupancy genome-wide. The factorFootprints function uses matchPWM
    # to predict the binding sites using the input position weight matrix (PWM). Then it calculates and plots
    # the accumulated coverage for those binding sites to show the status of the occupancy genome-wide.
    # Unlike CENTIPEDE4, the footprints generated here do not take the conservation (PhyloP) into consideration.
    # factorFootprints function could also accept the binding sites as a GRanges object.
    ####################################################################################################

    CTCF <- query(MotifDb, c(input$motif_value))
    CTCF <- as.list(CTCF)
    print(CTCF[[1]], digits = 2)



    # library(BSgenome.Hsapiens.UCSC.hg19)
    # library(input$bs_genome_input, character.only = T)


    genome <- get(unlist(strsplit(input$bs_genome_input, "\\."))[[2]])





    js$addStatusIcon("nucleosomepositioning_tab", "done")
    isolate({
        my_values$done <- TRUE
    })

    return(list("gal1" = gal1, "txs" = txs, "objs" = objs, "tss_sigs" = tss_sigs, "tss_sigs.log2" = tss_sigs.log2, "TSS" = TSS, "dws" = dws, "ups" = ups, "NTILE" = NTILE, "genome" = genome, "CTCF" = CTCF))
})

output$empty_txt_output <- renderText({
    inputDataReactive()

    ""
})



output$plot_pt_score <- renderPlot({
    # files will be output into outPath
    inputDataReactive()
    pt <- my_values$pt
    plot(pt$log2meanCoverage, pt$PT_score,
        xlab = "log2 mean coverage",
        ylab = "Promoter vs Transcript"
    )
})


output$plot_nfr_score <- renderPlot({
    inputDataReactive()
    nfr <- my_values$nfr
    plot(nfr$log2meanCoverage, nfr$NFR_score,
        xlab = "log2 mean coverage",
        ylab = "Nucleosome Free Regions score",
        main = "NFRscore for 200bp flanking TSSs",
        xlim = c(-10, 0), ylim = c(-5, 5)
    )
})


output$plot_tssre_score <- renderPlot({
    inputDataReactive()
    tsse <- my_values$tsse
    plot(100 * (-9:10 - .5), tsse$values,
        type = "b",
        xlab = "distance to TSS",
        ylab = "aggregate TSS score"
    )
})


output$plotCumulativePercentage <- renderPlot({
    inputDataReactive()
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
    # cumulativePercentage(bamfiles[1:2], as(seqinformation[input$sel_chromosome], "GRanges"))
    cumulativePercentage(bamfiles[1:2], my_values$GRanges)
})

output$plot_tss_featureAlignedHeatmap <- renderPlot({
    tss_sigs.log2 <- inputDataReactive()$tss_sigs.log2
    NTILE <- inputDataReactive()$NTILE
    tss_center_peaks <- my_values$tss_center_peaks

    featureAlignedHeatmap(tss_sigs.log2, tss_center_peaks, zeroAt = .5, n.tile = NTILE)



    # featureAlignedHeatmap(sigs$signal,
    #         feature.gr=reCenterPeaks(sigs$bindingSites,
    #                                 width=200+width(sigs$bindingSites[1])),
    #         annoMcols="score",
    #         sortBy="score",
    #         n.tile=ncol(sigs$signal[[1]]))
})



output$plot_signals <- renderPlot({
    inputDataReactive()
    out <- my_values$out
    matplot(out,
        type = "l", xaxt = "n",
        xlab = "Position (bp)",
        ylab = "Fraction of signal"
    )
    axis(1,
        at = seq(0, 100, by = 10) + 1,
        labels = c("-1K", seq(-800, 800, by = 200), "1K"), las = 2
    )
    abline(v = seq(0, 100, by = 10) + 1, lty = 2, col = "gray")
})



output$plot_Footprints <- renderPlot({
    isolate({
        seqlev <- input$sel_chromosome
    })

    CTCF <- inputDataReactive()$CTCF
    genome <- inputDataReactive()$genome

    outPath <- my_values$outPath
    shiftedBamfile <- file.path(outPath, "shifted.bam")

    my_values$binding_sites_sigs <- factorFootprints(shiftedBamfile,
        pfm = CTCF[[1]],
        genome = genome, ## Don't have a genome? ask ?factorFootprints for help
        min.score = "90%", seqlev = seqlev,
        upstream = 100, downstream = 100
    )
})


output$plot_binding_sites_featureAlignedHeatmap <- renderPlot({
    inputDataReactive()
    sigs <- my_values$binding_sites_sigs
    NTILE <- inputDataReactive()$NTILE
    tss_center_peaks <- my_values$tss_center_peaks

    
    featureAlignedHeatmap(sigs$signal,
            feature.gr=reCenterPeaks(sigs$bindingSites,
                                    width=200+width(sigs$bindingSites[1])),
            annoMcols="score",
            sortBy="score",
            n.tile=ncol(sigs$signal[[1]]))
})




output$plot_vp <- renderPlot({
    isolate({
        seqlev <- input$sel_chromosome
    })
    inputDataReactive()

    outPath <- my_values$outPath
    shiftedBamfile <- file.path(outPath, "shifted.bam")

    CTCF <- inputDataReactive()$CTCF
    genome <- inputDataReactive()$genome
    my_values$vp <- vPlot(shiftedBamfile,
        pfm = CTCF[[1]],
        genome = genome, min.score = "90%", seqlev = seqlev,
        upstream = 200, downstream = 200,
        ylim = c(30, 250), bandwidth = c(2, 1)
    )
})

output$plot_distanceDyad <- renderPlot({
    inputDataReactive()
    vp <- my_values$vp
    distanceDyad(vp, pch = 20, cex = .5)
})



output$bamfilesTable <- renderDataTable(
    {
        inputDataReactive()
        files <- list.files(my_values$outPath, full.names = T, recursive = TRUE)
        df <- data.frame(Filename=basename(files))
        DT::datatable(df, options = list(scrollX = TRUE))
    },
    options = list(scrollX = TRUE)
)

  output$download_bamfiles_btn <- downloadHandler(
    filename = function(){
      paste("bamfiles_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      files <- list.files(my_values$outPath, full.names = T, recursive = TRUE)
      files <- files[input$bamfilesTable_rows_selected]
      print(files)
      print(input$bamfilesTable_rows_selected)
    #   reactiveValuesToList(to_download) %>%
    #     imap(function(x,y){
    #       if(!is.null(x)){
    #         file_name <- glue("{y}_data.csv")
    #         readr::write_csv(x, file.path(temp_directory, file_name))
    #       }
    #     })
      
      file.copy(files, temp_directory)
    #   for(f in files){
    #     file_copy(f, paste0(temp_directory, basename(f)))
    #   }
     
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
      
      
    },
    contentType = "application/zip"
    
  )
