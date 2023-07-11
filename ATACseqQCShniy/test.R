## load the library
library(ATACseqQC)
library(BSgenome.Mmusculus.UCSC.mm10)
library(TxDb.Mmusculus.UCSC.mm10.knownGene)

library(BSgenome.Hsapiens.UCSC.hg19)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(ggplot2)
library(dplyr)
library(Rsamtools)
library(GenomicAlignments)


#isDuplicate, it is a flag in the BAM alignment records that
#indicates whether a read is marked as a duplicate. 
#Duplicates can occur during library preparation or sequencing, and they are typically
#identified and marked during data processing to avoid bias in downstream analyses.


## input the bamFile from the ATACseqQC package 


bamfile <- system.file("extdata", "GL1.bam", 
                        package="ATACseqQC", mustWork=TRUE)

                        
bamfile <- '/Users/nr83/Downloads/Sammy/Sammy_KO_ATAC_S37_withrg.csorted.cleaned.aligned.bam'

bamfile <- '/Users/nr83/Downloads/Sammy_v2/t.bam'


gal <- readGAlignments(bamfile)

gr <- GRanges(seqnames = 'chr1', ranges=ranges(gal)[1:5])
seq_gal <- readGAlignments(bamfile,param = ScanBamParam(which=gr, what=c('seq','qual')))

mappedReads <- idxstatsBam(bamfile)
mappedReads <- mappedReads %>% filter(mapped > 0) %>% arrange(desc(mapped)) %>% head(30)

df$Name <- reorder(mappedReads$Name, df$Age) 


ggplot(mappedReads, aes(reorder(seqnames,mapped) , mapped, fill=seqnames)) + geom_bar(stat="identity") + coord_flip()
bamfile.labels <- gsub(".bam", "", basename(bamfile))



estimateLibComplexity(readsDupFreq(bamfile))

fragSize <- fragSizeDist(bamfile, bamfile.labels)


possibleTag <- list("integer"=c("AM", "AS", "CM", "CP", "FI", "H0", "H1", "H2", 
                                "HI", "IH", "MQ", "NH", "NM", "OP", "PQ", "SM",
                                "TC", "UQ"), 
                    "character"=c("BC", "BQ", "BZ", "CB", "CC", "CO", "CQ", "CR",
                                  "CS", "CT", "CY", "E2", "FS", "LB", "MC", "MD",
                                  "MI", "OA", "OC", "OQ", "OX", "PG", "PT", "PU",
                                  "Q2", "QT", "QX", "R2", "RG", "RX", "SA", "TS",
                                  "U2"))
library(Rsamtools)
bamTop100 <- scanBam(BamFile(bamfile, yieldSize = 100),
                     param = ScanBamParam(tag=unlist(possibleTag)))[[1]]$tag
tags <- names(bamTop100)[lengths(bamTop100)>0]
tags


header <- scanBamHeader(bamfile)
print('header')

# Extract the chromosome names from the header

print(header[[bamfile]]$targets)
chromosomes <- names(header[[bamfile]]$targets)



## files will be output into outPath
outPath <- "splited"
dir.create(outPath)
## shift the coordinates of 5'ends of alignments in the bam file

## if you don't have an available TxDb, please refer
## GenomicFeatures::makeTxDbFromGFF to create one from gff3 or gtf file.
seqlev <- "chr1" ## subsample data for quick run

seqinformation <- seqinfo(TxDb.Mmusculus.UCSC.mm10.knownGene)
seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)


## files will be output into outPath
outPath <- "splited"
dir.create(outPath)
## shift the coordinates of 5'ends of alignments in the bam file
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
## if you don't have an available TxDb, please refer
## GenomicFeatures::makeTxDbFromGFF to create one from gff3 or gtf file.
seqlev <- "chr1" ## subsample data for quick run
seqinformation <- seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene)
which <- as(seqinformation[seqlev], "GRanges")
gal <- readBamFile(bamfile, tag=tags, which=which, asMates=TRUE, bigFile=TRUE)
shiftedBamfile <- file.path(outPath, "shifted.bam")
gal1 <- shiftGAlignmentsList(gal, outbam=shiftedBamfile)



library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txs <- transcripts(TxDb.Hsapiens.UCSC.hg19.knownGene)
pt <- PTscore(gal1, txs)
plot(pt$log2meanCoverage, pt$PT_score, 
     xlab="log2 mean coverage",
     ylab="Promoter vs Transcript")



tags <- c("AS", "XN", "XM", "XO", "XG", "NM", "MD", "YS", "YT")
which <- as(seqinformation[seqlev], "GRanges")
which <- as(seqinfo(Mmusculus)["chr1"], "GRanges")
gal <- readBamFile(bamfile, tag=tags, which=which,what=c("qname", "flag", "mapq", "seq", "qual"), asMates=TRUE, bigFile=TRUE)
gal1 <- shiftGAlignments(gal, outbam=shiftedBamfile)



header1 <- scanBamHeader(shiftedBamfile)
print(header1)

# Extract the chromosome names from the header

print(header1[[shiftedBamfile]]$targets)
chromosomes <- names(header1[[shiftedBamfile]]$targets)









library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txs <- transcripts(TxDb.Hsapiens.UCSC.hg19.knownGene)
pt <- PTscore(gal1, txs)
plot(pt$log2meanCoverage, pt$PT_score, 
     xlab="log2 mean coverage",
     ylab="Promoter vs Transcript")


nfr <- NFRscore(gal1, txs)
plot(nfr$log2meanCoverage, nfr$NFR_score, 
     xlab="log2 mean coverage",
     ylab="Nucleosome Free Regions score",
     main="NFRscore for 200bp flanking TSSs",
     xlim=c(-10, 0), ylim=c(-5, 5))



tsse <- TSSEscore(gal1, txs)
tsse$TSSEscore
plot(100*(-9:10-.5), tsse$values, type="b", 
     xlab="distance to TSS",
     ylab="aggregate TSS score")


library(BSgenome.Hsapiens.UCSC.hg19)
library(phastCons100way.UCSC.hg19)
## run program for chromosome 1 only
txs <- txs[seqnames(txs) %in% "chr1"]
genome <- Hsapiens
## split the reads into NucleosomeFree, mononucleosome, 
## dinucleosome and trinucleosome.
## and save the binned alignments into bam files.
objs <- splitGAlignmentsByCut(gal1, txs=txs, genome=genome, outPath = outPath,
                              conservation=phastCons100way.UCSC.hg19)
## list the files generated by splitGAlignmentsByCut.
dir(outPath)




library(ChIPpeakAnno)
bamfiles <- file.path(outPath,
                     c("NucleosomeFree.bam",
                     "mononucleosome.bam",
                     "dinucleosome.bam",
                     "trinucleosome.bam"))
## Plot the cumulative percentage of tag allocation in nucleosome-free 
## and mononucleosome bam files.
cumulativePercentage(bamfiles[1:2], as(seqinformation["chr1"], "GRanges"))




TSS <- promoters(txs, upstream=0, downstream=1)
TSS <- unique(TSS)
## estimate the library size for normalization
(librarySize <- estLibSize(bamfiles))



NTILE <- 101
dws <- ups <- 1010
sigs <- enrichedFragments(gal=objs[c("NucleosomeFree", 
                                     "mononucleosome",
                                     "dinucleosome",
                                     "trinucleosome")], 
                          TSS=TSS,
                          librarySize=librarySize,
                          seqlev=seqlev,
                          TSS.filter=0.5,
                          n.tile = NTILE,
                          upstream = ups,
                          downstream = dws)
## log2 transformed signals
sigs.log2 <- lapply(sigs, function(.ele) log2(.ele+1))
#plot heatmap
featureAlignedHeatmap(sigs.log2, reCenterPeaks(TSS, width=ups+dws),
                      zeroAt=.5, n.tile=NTILE)



out <- featureAlignedDistribution(sigs, 
          reCenterPeaks(TSS, width=ups+dws),
          zeroAt=.5, n.tile=NTILE, type="l", 
          ylab="Averaged coverage")


## rescale the nucleosome-free and nucleosome signals to 0~1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
out <- apply(out, 2, range01)
matplot(out, type="l", xaxt="n", 
        xlab="Position (bp)", 
        ylab="Fraction of signal")
axis(1, at=seq(0, 100, by=10)+1, 
     labels=c("-1K", seq(-800, 800, by=200), "1K"), las=2)
abline(v=seq(0, 100, by=10)+1, lty=2, col="gray")




## foot prints
library(MotifDb)
CTCF <- query(MotifDb, c("CTCF"))
CTCF <- as.list(CTCF)
print(CTCF[[1]], digits=2)




sigs <- factorFootprints(shiftedBamfile, pfm=CTCF[[1]], 
                         genome=genome, ## Don't have a genome? ask ?factorFootprints for help
                         min.score="90%", seqlev=seqlev,
                         upstream=100, downstream=100)



featureAlignedHeatmap(sigs$signal, 
                      feature.gr=reCenterPeaks(sigs$bindingSites,
                                               width=200+width(sigs$bindingSites[1])), 
                      annoMcols="score",
                      sortBy="score",
                      n.tile=ncol(sigs$signal[[1]]))

sigs$Profile.segmentation




vp <- vPlot(shiftedBamfile, pfm=CTCF[[1]], 
            genome=genome, min.score="90%", seqlev=seqlev,
            upstream=200, downstream=200, 
            ylim=c(30, 250), bandwidth=c(2, 1))


distanceDyad(vp, pch=20, cex=.5)