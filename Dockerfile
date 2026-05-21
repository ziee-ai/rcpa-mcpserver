FROM bioconductor/bioconductor_docker:RELEASE_3_20

ENV RCPA_PORT=9004 \
    RCPA_STATIC_PORT=9005 \
    RCPA_DAEMONS=6 \
    RCPA_RESULTS_DIR=/var/lib/rcpa/results \
    BASE_URL=http://localhost:9005 \
    DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev \
      libgit2-dev libfontconfig1-dev libfreetype6-dev libharfbuzz-dev \
      libfribidi-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "BiocManager::install(c('SummarizedExperiment','limma','DESeq2','edgeR','S4Vectors','AnnotationDbi','Biobase','GEOquery','ROntoTools','fgsea','GSA'), ask=FALSE, update=FALSE)"

RUN R -e "install.packages(c('mirai','nanonext','processx','httr2','jsonlite','jsonvalidate','jose','later','promises','R6','openssl','testthat','withr','mockery','tidyr','dplyr','ggplot2','stringr','ggnewscale','ggrepel','ggpattern','scales','RobustRankAggreg','rlang','png','ggvenn','CePa','meta'), repos='https://cloud.r-project.org')"

RUN R -e "install.packages('RCPA', repos='https://cloud.r-project.org')"

COPY mcpserver_*.tar.gz /tmp/mcpserver.tar.gz
RUN R CMD INSTALL /tmp/mcpserver.tar.gz && rm /tmp/mcpserver.tar.gz

COPY . /tmp/rcpa-mcpserver
RUN R CMD INSTALL /tmp/rcpa-mcpserver && rm -rf /tmp/rcpa-mcpserver

RUN mkdir -p /var/lib/rcpa/results
VOLUME ["/var/lib/rcpa/results"]

EXPOSE 9004 9005

CMD ["Rscript", "-e", "rcpa.mcpserver::run_http_entrypoint()"]
