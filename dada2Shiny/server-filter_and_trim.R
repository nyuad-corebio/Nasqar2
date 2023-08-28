reactiveInputData <- eventReactive(input$runQc, {
    js$addStatusIcon("filter_and_trim_tab", "loading")
    req(input$sel_sample_qualityprofile_tab)
    path <- my_values$base_dir
    sample.names <- row.names(my_values$samples_df)

    print(my_values$samples_df)
    fnFs <- file.path(path, my_values$samples_df[, "FASTQ_Fs"])
    fnRs <- file.path(path, my_values$samples_df[, "FASTQ_Rs"])
    # print(fnFs)
    # print(fnRs)
    filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
    filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))

    print("dir1")
    sapply(fnFs, function(fasqfile) {
        print(file.exists(fasqfile))
    })
    print("dir2")
    sapply(fnRs, function(fasqfile) {
        print(file.exists(fasqfile))
    })
    names(filtFs) <- sample.names
    names(filtRs) <- sample.names


    withProgress(message = "Running DADA2 , please wait", {
      
        shiny::setProgress(value = 0.1, detail = "...filterAndTrim")


        out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,
            truncLen = c(240, 160),
            maxN = 0, maxEE = c(2, 2), truncQ = 2, rm.phix = TRUE,
            compress = TRUE, multithread = TRUE
        )

        shiny::setProgress(value = 0.4, detail = "...learnErrors")
        errF <- learnErrors(filtFs, multithread = TRUE)
        errR <- learnErrors(filtRs, multithread = TRUE)

        shiny::setProgress(value = 0.6, detail = "...dadafs")
        dadaFs <- dada(filtFs, err = errF, multithread = TRUE)
        dadaRs <- dada(filtRs, err = errR, multithread = TRUE)

        shiny::setProgress(value = 0.7, detail = "...mergePairs")
        mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose = TRUE)
        # Inspect the merger data.frame from the first sample
        print('mergers')

        
        
        print(names(head(mergers)))
        updateSelectInput(session, "selSample4margePairedReadsTab", choices = names(mergers))


        shiny::setProgress(value = 0.8, detail = "...makeSequenceTable")
        seqtab <- makeSequenceTable(mergers)
        dim(seqtab)

        # Inspect distribution of sequence lengths
        print('table')
        seqtabTable <- table(nchar(getSequences(seqtab)))


        seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = TRUE, verbose = TRUE)
        dim(seqtab.nochim)

        sum(seqtab.nochim) / sum(seqtab)

        getN <- function(x) sum(getUniques(x))
        track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
        # If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
        colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
        rownames(track) <- sample.names
        # updateSelectInput(session, "selSample4trackReadsTab", choices = sample.names)


        print('track')
        print(head(track))

        shiny::setProgress(value = 1.0, detail = "...done")
       
    })
     js$addStatusIcon("filter_and_trim_tab", "done")
    return(list(out = out, errF= errF, errR=errF, mergers = mergers, seqtabTable=seqtabTable, track=track ))
})



output$filterAndTrim_output_table <- DT::renderDataTable(
    {
        # print(filtFs)

        data <- reactiveInputData()

        data$out
    },
    options = list(scrollX = TRUE, pageLength = 15)
)
