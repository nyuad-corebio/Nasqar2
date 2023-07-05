output$plot_libcomplexity <-  renderPlot({
    print(input$sample_librarycomplexity)
    print(my_values$samples_df[input$sample_librarycomplexity, 'BamFile'])

    bamfile <- file.path(my_values$base_dir, my_values$samples_df[input$sample_librarycomplexity, 'BamFile'])
    print(bamfile)
    estimateLibComplexity(readsDupFreq(bamfile ))
})

  text_reactive <- observeEvent( input$submit, {
    print('submit')
  })
