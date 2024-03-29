% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agCount_Label_Num_Wikidata_P279_P31.R
\name{agCount_Label_Num_Wikidata_P279_P31}
\alias{agCount_Label_Num_Wikidata_P279_P31}
\title{Count triples from a label query for Wikidata local endpoint via SPARQL.}
\usage{
agCount_Label_Num_Wikidata_P279_P31(Entity_Name, Dir = tempdir())
}
\arguments{
\item{Entity_Name}{a character vector. The string was
automatically judged to be Japanese (@ja) or English (@en)}

\item{Dir}{a folder path for output files.}
}
\value{
data.frame
}
\description{
This function allow to count labels and class relations for
wikidata local endpoint at OECU via SPARQL.
For parameters of EndPoint and graph id, the variable of KzLabEndPoint_Wikidata is used.
For parameters of properties, the variable of wikidataClassProperty is used.
}
\examples{
\dontrun{
#parameters

#polymer (wikidata prefix URI: wd:Q81163)
Entity_Name <- "polymer"

#run SPARQL
agCount_Label_Num_Wikidata_P279_P31(
  Entity_Name=Entity_Name,
  Message=TRUE
  )

#show the SPARQL query
CkeckQuery_agCount_Label_Num_Wikidata_P279_P31(
  Entity_Name=Entity_Name,
  Message=TRUE
  )

#Parallel processing of 4 cores using furrr package
library(furrr)
plan(multisession(workers = 4))
#plan()

#prepare a vector of labels
Labs <- c("polymer", "protein", "Pteridophyta", "material substance")

#run multisession
results <- future_map(Labs, agCount_Label_Num_Wikidata_P279_P31, .progress=T)
results

}
}
\author{
Satoshi Kume
}
