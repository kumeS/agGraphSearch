#https://hub.docker.com/layers/rocker/rstudio/4.2.2/images/sha256-d0da273d992df6610bfad93095d1be9c3faed40f29138f36f80b6e44ec59ea56?context=explore
FROM rocker/rstudio:4.2.2

WORKDIR /home/rstudio

COPY --chown=rstudio:rstudio . /home/rstudio/

RUN sudo apt-get update -y && sudo apt-get install -y pciutils libfftw3-dev libfftw3-doc r-base-dev libcurl4-openssl-dev libxml2-dev libssl-dev libmagick++-6.q16-dev graphviz graphviz-dev

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages(c('R.cache', 'knitr', 'rmarkdown', 'reticulate', 'remotes', 'XML', 'RCurl'))"

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages('https://cran.r-project.org/src/contrib/Archive/SPARQL/SPARQL_1.16.tar.gz', repos=NULL, type='source'); library(SPARQL)"

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages( 'devtools' ); devtools::install_github( 'kumeS/agGraphSearch' , force = TRUE); library( 'agGraphSearch' )"

