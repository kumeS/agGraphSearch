if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}
library(WikidataQueryServiceR)

#QIDname="leukemia"; language=1
#searchQID(QIDname="leukemia", language=1)

##' Import multiple KEGG pathways and integrate the pathways
##' into Cy3D renderer
##'
##' @title Create 3D network view for transomics visualization.
##' @param QIDname a character vector of entity label used in SPARQL search.
##' @param language a language of labels used in searches.
##'  If the value is 1, the english is used.
##'
##'
##' @param transomicEdges Path of a TSV file with the 5 columns
##' (layer index of a source node,
##' name or KEGG object ID that the source node should have,
##' layer index of a target node,
##' name or KEGG object ID that the target node should have,
##' interaction type).
##' @param stylexml Path of a XML file for Cytoscape style
##' @return A SUID of the 3D network.
##' @author Satoshi Kume
##' @import WikidataQueryServiceR
##' @export
##' @examples \dontrun{
##' # search the QID for "leukemia"
##' searchQID(QIDname="leukemia")
##' }
##'
##'

searchQID <- function(QIDname, language=1){
LABEL <- QIDname
if(language == 1){
  rdfs.l <- "en"
} else {
if(language == 2){
  rdfs.l <- "ja"
}else{
  rdfs.l <- "en"}
}

Prefix <- '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
'

Query00 <-paste('
SELECT distinct ?qid ?qidLabel
WHERE
{
?qid rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")

message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA00 <- query_wikidata(paste(Prefix, Query00), format = "simple"), silent = T))

Query01 <-paste('
SELECT distinct ?qid ?qidLabel
WHERE
{
?qid skos:altlabel \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")
suppressMessages(try(SPA01 <- query_wikidata(paste(Prefix, Query01), format = "simple"), silent = T))

try(SPA <- rbind(SPA00, SPA01), silent=T)
if(exists("SPA")){}else{return(message("No Data"))}

try(SPA$qid <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$qid), silent = T)
try(SPA <- SPA[as.numeric(rownames(unique(SPA["qid"]))),], silent = T)

Lab <- grep(pattern="^wd:P", SPA$qid)
if(length(Lab) != 0){SPA <- SPA[-Lab,]}
return(data.frame(SPA, stringsAsFactors = F))
}
