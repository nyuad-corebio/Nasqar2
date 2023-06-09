## ==================================================================================== ##
# START Shiny App for analysis and visualization of transcriptome data.
# Copyright (C) 2016  Jessica Minnier
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# You may contact the author of this code, Jessica Minnier, at <minnier@ohsu.edu>
## ==================================================================================== ##
##
##
# update list of groups
observe({
    print("server-samplegroupplots-update")
    data_analyzed <- analyzeDataReactive()
    tmpgroups <- data_analyzed$group_names
    tmpsamples <- colnames(data_analyzed$expr_data)
    updateSelectizeInput(session, "sampleres_groups",
        choices = tmpgroups, selected = tmpgroups
    )
    updateSelectizeInput(session, "sampleres_samples",
        choices = tmpsamples, selected = tmpsamples
    )
})

# sampleres_groups = intersect selected groups with sample names
observe({
    print("server-sampleplots-update-samples")
    data_analyzed <- analyzeDataReactive()
    tmpgroups <- input$sampleres_groups
    tmpdat <- data_analyzed$sampledata %>% filter(group %in% tmpgroups)
    tmpsamples <- as.character(tmpdat$sampleid)
    updateSelectizeInput(session, "sampleres_samples",
        choices = tmpsamples, selected = tmpsamples
    )
})

observe({
    output$gene_pheatmap <- renderPlot({
        print("render gene_pheatmap")
        data_analyzed <- analyzeDataReactive()
        data_results <- data_analyzed$results
        geneids <- data_analyzed$geneids
        sampledata <- data_analyzed$sampledata

        tmpgroups <- input$sampleres_groups
        tmpsamples <- input$sampleres_samples
        tmpkeep <- which((sampledata$group %in% tmpgroups) & (sampledata$sampleid %in% tmpsamples))

        if (length(tmpkeep) > 0) {
            tmpdat <- data_analyzed$expr_data[, tmpkeep]
            gene_pheatmap(as.matrix(tmpdat),
                sampleid = sampledata$sampleid[tmpkeep], annotation_row = sampledata[tmpkeep, "group", drop = FALSE]
            )
        }
    })

    output$gene_pheatmap_deseq <- renderPlot({
        print("render gene_pheatmap_deseq")
        data_analyzed <- analyzeDataReactive()
        sampledata <- data_analyzed$sampledata
        dds <- data_analyzed$dds

        tmpgroups <- input$sampleres_groups
        tmpsamples <- input$sampleres_samples
        tmpkeep <- which((sampledata$group %in% tmpgroups) & (sampledata$sampleid %in% tmpsamples))

        if (length(tmpkeep) > 0) {
            withProgress(message = "Drawing heatmap (DESeq), please wait", {
                tmpdat <- dds[, tmpkeep]
                setProgress(0.5)
                gene_pheatmap_deseq(tmpdat,
                    sampleid = sampledata$sampleid[tmpkeep], annotation_row = sampledata[tmpkeep, "group", drop = FALSE]
                )

                setProgress(1)
            })
        }
    })

    output$pca_plot <- renderPlot({
        print("render PCA plot")

        data_analyzed <- analyzeDataReactive()
        data_results <- data_analyzed$results
        geneids <- data_analyzed$geneids
        sampledata <- data_analyzed$sampledata


        tmpgroups <- input$sampleres_groups
        tmpsamples <- input$sampleres_samples
        tmpkeep <- which((sampledata$group %in% tmpgroups) & (sampledata$sampleid %in% tmpsamples))

        if (length(tmpkeep) > 0) {
            tmpdat <- data_analyzed$expr_data[, tmpkeep]
            validate(need(length(input$pcnum) == 2, message = "Select 2 Prinical Components."))
            gene_pcaplot(tmpdat,
                sampleid = sampledata$sampleid[tmpkeep],
                groupdat = sampledata[tmpkeep, "group", drop = FALSE],
                pcnum = as.numeric(input$pcnum),
                colorfactor = "group"
            )
        }
    })




    # rna_volcanoplot(data_results = data_results,
    #                 group_sel = input$analysisres_groups,
    #                 absFCcut = input$analysisres_fold_change_cut,
    #                 fdrcut = input$analysisres_fdrcut)%>%
    #   bind_shiny("volcanoplot_2groups_ggvis","volcanoplot_2groups_ggvisUI")
})
