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
gene_pheatmap <- function(exprdat, sampleid, annotation_row = NULL) {
    sampleDists <- dist(t(exprdat))

    sampleDistMatrix <- as.matrix(sampleDists)
    rownames(sampleDistMatrix) <- sampleid
    if (!is.null(annotation_row)) rownames(annotation_row) <- sampleid
    colnames(sampleDistMatrix) <- NULL
    colors <- colorRampPalette(rev(brewer.pal(9, "Blues")))(255)
    pheatmap::pheatmap(sampleDistMatrix,
        clustering_distance_rows = sampleDists,
        clustering_distance_cols = sampleDists,
        annotation_row = annotation_row,
        col = colors
    )
}


gene_pheatmap_deseq <- function(exprdat, sampleid, annotation_row = NULL) {
    rld <- rlog(exprdat)
    sampleDists <- dist(t(assay(rld)))

    sampleDistMatrix <- as.matrix(sampleDists)
    rownames(sampleDistMatrix) <- sampleid
    if (!is.null(annotation_row)) rownames(annotation_row) <- sampleid
    colnames(sampleDistMatrix) <- NULL
    colors <- colorRampPalette(rev(brewer.pal(9, "Blues")))(255)
    pheatmap::pheatmap(sampleDistMatrix,
        clustering_distance_rows = sampleDists,
        clustering_distance_cols = sampleDists,
        annotation_row = annotation_row,
        col = colors
    )
}

gene_pcaplot <- function(exprdat, sampleid, groupdat = NULL, colorfactor = NULL, shapefactor = NULL,
                         plot_sampleids = TRUE, pcnum = 1:2, plottitle = "PCA Plot") {
    # adapted from DESeq2:::plotPCA.DESeqTransform
    pca <- prcomp(t(exprdat))

    percentVar <- pca$sdev^2 / sum(pca$sdev^2)

    if (is.null(groupdat)) groupdat <- data.frame("group" = rep(1, ncol(exprdat)))

    intgroup <- colnames(groupdat)

    allgroup <- if (length(intgroup) > 1) {
        factor(apply(groupdat, 1, paste, collapse = ":"))
    } else {
        allgroup <- intgroup
    }

    d <- data.frame(
        PC1 = pca$x[, pcnum[1]], PC2 = pca$x[, pcnum[2]], uniquegroup = allgroup,
        groupdat, name = sampleid
    )

    percentVar <- round(100 * percentVar)

    if (is.null(colorfactor)) {
        d$color <- as.factor(1)
    } else {
        colnames(d)[colnames(d) == colorfactor] <- "color"
    }

    if (is.null(shapefactor)) {
        d$shape <- as.factor(1)
    } else {
        colnames(d)[colnames(d) == shapefactor] <- "shape"
    }

    if (identical(shapefactor, colorfactor)) {
        d$shape <- d$color
    }

    p <- ggplot(d, aes(PC1, PC2, color = color, shape = shape, size = 3), label.repel = T)


    if (plot_sampleids) {
        p <- p + geom_text_repel(aes(label = name)) + geom_point()
    } else {
        p <- p + geom_point()
    }

    if (!is.null(colorfactor)) {
        p <- p + guides(color = guide_legend(title = colorfactor))
    } else {
        p <- p + guides(color = "none")
    }
    if (!is.null(shapefactor)) {
        p <- p + guides(shape = guide_legend(title = shapefactor))
    } else {
        p <- p + guides(shape = "none")
    }
    p <- p + guides(size = "none") + theme_bw() +
        xlab(paste0("PC", pcnum[1], ": ", percentVar[pcnum[1]], "% variance")) +
        ylab(paste0("PC", pcnum[2], ": ", percentVar[pcnum[2]], "% variance")) + ggtitle(plottitle)

    # increase legend font size
    p <- p + theme(legend.text = element_text(size = 18)) + theme(legend.title = element_text(size = 18))


    return(p)
}
