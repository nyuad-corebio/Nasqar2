output$plotErrors_errF <-  renderPlot({
   
    errF<- reactiveInputData()$errF
    print('plotErrors_errF')
    print(errF)
    
    plotErrors(errF, nominalQ=TRUE)
})

output$plotErrors_errR <-  renderPlot({
   


    errR<- reactiveInputData()$errR
     print('plotErrors_errR')
    
    plotErrors(errR, nominalQ=TRUE)
})
