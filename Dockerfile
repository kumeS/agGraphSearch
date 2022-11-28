FROM rocker/rstudio:4.1.1

WORKDIR /home/rstudio

COPY --chown=rstudio:rstudio . /home/rstudio/

RUN sudo apt-get update -y && sudo apt-get install -y pciutils libfftw3-dev libfftw3-doc r-base-dev libcurl4-openssl-dev libxml2-dev libssl-dev libmagick++-6.q16-dev graphviz graphviz-dev

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages(c('R.cache', 'knitr', 'rmarkdown', 'reticulate', 'remotes', 'XML', 'RCurl', 'pkgdown'))"

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages('https://cran.r-project.org/src/contrib/Archive/SPARQL/SPARQL_1.16.tar.gz', repos=NULL, type='source'); library(SPARQL)"

RUN Rscript -e "options(repos = c(CRAN = 'https://cran.r-project.org')); install.packages( 'devtools', dependencies = TRUE); devtools::install_github( 'kumeS/agGraphSearch' , force = TRUE); library( 'agGraphSearch' )"

