FROM rocker/shiny:4.2.3

RUN apt-get update \
  && apt-get install vim libjpeg9  libtiff5-dev cmake  libmagick++-dev -y --no-install-recommends \
  && apt-get autoremove -yqq --purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ENV TZ=Asia/Dubai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

USER shiny
WORKDIR /home/shiny
RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /home/shiny/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh
ENV PATH="/home/shiny/miniconda3/bin:$PATH"
RUN conda install -c bioconda -c conda-forge mamba -y
ADD environment.yaml environment.yaml
RUN mamba env create -f environment.yaml
RUN echo "alias l='ls -lah'" >> ~shiny/.bashrc
RUN conda init bash
SHELL ["/bin/bash", "-c"]
RUN echo "conda activate nasqar_env" >> ~shiny/.bashrc
RUN conda clean --all -y


RUN conda run -n nasqar_env R -e "install.packages(c('stringi', 'GOplot', 'wordcloud2'), repos='http://cran.rstudio.com/')"
RUN conda run -n nasqar_env R -e "devtools::install_github('YuLab-SMU/clusterProfiler')"
RUN conda run -n nasqar_env R -e "BiocManager::install(c('org.Hs.eg.db','org.Mm.eg.db','org.Rn.eg.db','org.Sc.sgd.db','org.Dm.eg.db','org.At.tair.db','org.Dr.eg.db','org.Bt.eg.db','org.Ce.eg.db','org.Gg.eg.db','org.Cf.eg.db','org.Ss.eg.db','org.Mmu.eg.db','org.EcK12.eg.db','org.Xl.eg.db','org.Pt.eg.db','org.Ag.eg.db','org.Pf.plasmo.db','org.EcSakai.eg.db'))"
RUN conda run -n nasqar_env R -e "devtools::install_github('smin95/smplot2')"
##Seurat wizard
RUN conda run -n nasqar_env R -e "devtools::install_github('nyuad-corebio/seuratv3wizard')"
RUN conda run -n nasqar_env R -e "BiocManager::install(c('ATACseqQC'))"
RUN conda run -n nasqar_env R -e "BiocManager::install(c('ChIPpeakAnno', 'MotifDb', 'GenomicAlignments'))"

RUN conda run -n nasqar_env R -e "BiocManager::install( c('BSgenome.Hsapiens.UCSC.hg19'), ask = FALSE, update = FALSE)"
RUN conda run -n nasqar_env R -e "BiocManager::install( c('TxDb.Hsapiens.UCSC.hg19.knownGene'), ask = FALSE, update = FALSE)"

RUN conda run -n nasqar_env R -e "BiocManager::install( c('BSgenome.Celegans.UCSC.ce11'), ask = FALSE, update = FALSE)"
RUN conda run -n nasqar_env R -e "BiocManager::install( c('TxDb.Celegans.UCSC.ce11.refGene'), ask = FALSE, update = FALSE)"

RUN conda run -n nasqar_env R -e "BiocManager::install( c('BSgenome.Drerio.UCSC.danRer11'), ask = FALSE)"
RUN conda run -n nasqar_env R -e "BiocManager::install( c('TxDb.Drerio.UCSC.danRer11.refGene'), ask = FALSE, update = FALSE)"

RUN conda run -n nasqar_env R -e "BiocManager::install( c('BSgenome.Mmusculus.UCSC.mm10'), ask = FALSE, update = FALSE)"
RUN conda run -n nasqar_env R -e "BiocManager::install( c('TxDb.Mmusculus.UCSC.mm10.knownGene'), ask = FALSE, update = FALSE)"

RUN conda run -n nasqar_env R -e "BiocManager::install('compbiomed/animalcules')"


RUN conda run -n nasqar_env R -e "BiocManager::install(c('BiocGenerics', 'DelayedArray', 'DelayedMatrixStats', 'limma', 'lme4', 'S4Vectors', 'SingleCellExperiment', 'SummarizedExperiment', 'batchelor', 'HDF5Array', 'terra', 'ggrastr'))"
          
RUN conda run -n nasqar_env R -e "devtools::install_github('cole-trapnell-lab/monocle3')"
RUN conda run -n nasqar_env R -e "remotes::install_github('satijalab/seurat', 'seurat5', quiet = TRUE)"

USER root

RUN mkdir /opt/nasqar_build
#ADD NASQAR2/nasqar.tar /opt/nasqar_build/

COPY ClusterProfShinyGSEA /opt/nasqar_build/ClusterProfShinyGSEA
COPY ClusterProfShinyORA /opt/nasqar_build/ClusterProfShinyORA
COPY DEApp /opt/nasqar_build/DEApp
COPY deseq2shiny /opt/nasqar_build/deseq2shiny
COPY GeneCountMerger /opt/nasqar_build/GeneCountMerger
COPY NASQAR /opt/nasqar_build/NASQAR
COPY ATACseqQCShniy /opt/nasqar_build/ATACseqQCShniy
COPY dada2Shiny /opt/nasqar_build/dada2Shiny

USER shiny
WORKDIR /home/shiny
RUN mkdir ~/.ssh

USER root

RUN apt-get update \
  && apt-get install sshfs -y --no-install-recommends \
  && apt-get autoremove -yqq --purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*


WORKDIR /opt/nasqar_build
RUN wget https://github.com/UMMS-Biocore/debrowser/archive/refs/heads/master.zip
RUN unzip master.zip

RUN wget https://github.com/elliefewings/monocle3_shiny/archive/refs/heads/main.zip
RUN unzip monocle3_shiny-main.zip

RUN wget https://github.com/compbiomed/animalcules/archive/a24aced16297b12b92b63520b0046ddbae288322.zip 


RUN unzip a24aced16297b12b92b63520b0046ddbae288322.zip 
#WORKDIR /opt/nasqar_build/animalcules-a24aced16297b12b92b63520b0046ddbae288322
#RUN conda run -n nasqar_env R -e "devtools::install('.')"
#WORKDIR /opt/nasqar_build
RUN mv animalcules-a24aced16297b12b92b63520b0046ddbae288322/inst/shiny animalcules
RUN sed -i '1 i\library(animalcules)' animalcules/app.R



COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /opt/nasqar_build/shiny-server.sh
RUN mkdir /opt/nasqar_build/logs
RUN chown -R shiny:shiny /opt/nasqar_build/

USER shiny
RUN chmod a+x /opt/nasqar_build/shiny-server.sh

EXPOSE 3232

CMD ["/bin/bash", "/opt/nasqar_build/shiny-server.sh"]