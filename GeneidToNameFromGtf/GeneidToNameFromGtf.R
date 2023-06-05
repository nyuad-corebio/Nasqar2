if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("rtracklayer")

if(require("tidyverse")) {
    install.packages("tidyverse")
}

library(rtracklayer)
library(tydverse)

setwd(".")
gtf_file_path <- 'c_elegans.PRJNA13758.WS283.canonical_geneset.gtf'
gene_names_out_csv <- "gene_names.csv"

gtf <- import(gtf_file_path)
genenames <- (mcols(gtf)[,c("gene_id","gene_name")]) %>% na.omit()
gene_id <- as.data.frame(gene_id) %>% unique()
df<-merge(gene_id, genenames[, c("gene_id", "gene_name")], by="gene_id",all.x = TRUE)

write_csv(as.data.frame(df), gene_names_out_csv)
