output$plot_fragmentsize <-  renderPlot({
    print(input$sample_fragmentsize)
    print(my_values$samples_df[input$sample_fragmentsize, 'BamFile'])

    bamfile <- paste0(my_values$base_dir, '/',my_values$samples_df[input$sample_fragmentsize, 'BamFile'])
    print(bamfile)
    fragSize <- fragSizeDist(bamfile, input$sample_fragmentsize)
})