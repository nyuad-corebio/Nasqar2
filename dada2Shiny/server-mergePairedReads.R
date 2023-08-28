output$mergerTable <-  DT::renderDataTable({
            mergers<-reactiveInputData()$mergers
            print('mergerTable')
            # print(mergers)
            sample <- input$selSample4margePairedReadsTab
            merger <- mergers[[sample]]
            merger %>% relocate(sequence, .after = accept)
},options = list(scrollX = TRUE, pageLength = 15))