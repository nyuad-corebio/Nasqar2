output$plot_libcomplexity <-  renderPlot({
    print(my_values$bamfile)
    estimateLibComplexity(readsDupFreq(my_values$bamfile ))
})