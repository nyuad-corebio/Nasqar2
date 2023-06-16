output$plot_fragmentsize <-  renderPlot({
    bamfile <- my_values$bamfile
    bamfile.labels <- gsub(".bam", "", basename(bamfile))   
    fragSize <- fragSizeDist(bamfile, bamfile.labels)
})