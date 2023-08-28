output$plot_qualityprofile_fs <-  renderPlot({
    
    req(input$sel_sample_qualityprofile_tab)

    print(input$sel_sample_qualityprofile_tab)
    print(my_values$samples_df[input$sel_sample_qualityprofile_tab, 'FASTQ_Fs'])
    print('my_values$samples_df')
    print(my_values$samples_df)
    print(my_values$base_dir)
    fastq_file <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_qualityprofile_tab, 'FASTQ_Fs'])

    plotQualityProfile(fastq_file)
})

output$plot_qualityprofile_rs <-  renderPlot({
  req(input$sel_sample_qualityprofile_tab)
  
  
  print(input$sel_sample_qualityprofile_tab)
  print(my_values$samples_df[input$sel_sample_qualityprofile_tab, 'FASTQ_Rs'])
  
  fastq_file <- file.path(my_values$base_dir, my_values$samples_df[input$sel_sample_qualityprofile_tab, 'FASTQ_Rs'])
  print(fastq_file)
  plotQualityProfile(fastq_file)
})

  text_reactive <- observeEvent( input$submit, {
    print('submit')
  })
