FROM rocker/shiny

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
RUN conda run -n nasqar_env R -e "BiocManager::install(c('org.Dm.eg.db','org.Mm.eg.db'))"
RUN conda run -n nasqar_env R -e "devtools::install_github('smin95/smplot2')"
##Seurat wizard
RUN conda run -n nasqar_env R -e "devtools::install_github('nyuad-corebio/seuratv3wizard')"

USER root

RUN mkdir /opt/nasqar_build
#ADD NASQAR2/nasqar.tar /opt/nasqar_build/

COPY ClusterProfShinyGSEA /opt/nasqar_build/ClusterProfShinyGSEA
COPY ClusterProfShinyORA /opt/nasqar_build/ClusterProfShinyORA
COPY DEApp /opt/nasqar_build/DEApp
COPY deseq2shiny /opt/nasqar_build/deseq2shiny
COPY GeneCountMerger /opt/nasqar_build/GeneCountMerger
COPY NASQAR /opt/nasqar_build/NASQAR


WORKDIR /opt/nasqar_build
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /opt/nasqar_build/shiny-server.sh
RUN mkdir /opt/nasqar_build/logs
RUN chown -R shiny:shiny /opt/nasqar_build/

USER shiny
RUN chmod a+x /opt/nasqar_build/shiny-server.sh

EXPOSE 3232

CMD ["/bin/bash", "/opt/nasqar_build/shiny-server.sh"]
