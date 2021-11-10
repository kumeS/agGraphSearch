##' @title wikiCount_QID_Property_Object
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param Object
##' @param Property a character vector
##' @param Count a character vector
##' @param lang a numeric; The value 1 is to use "ja, en". The value 2 is to use "ja". The value 3 is to use "en".
##' @param GroupBy a logical
##'
##' @description this function is a general function for ...
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export wikiCount_QID_Property_Object
##' @importFrom WikidataQueryServiceR query_wikidata
##'


wikiCount_QID_Property_Object <- function(Entity_ID,
                                          Object,
                                          Property="?p",
                                          Count="?p",
                                          lang=1,
                                          GroupBy=FALSE){

QID <- Entity_ID
if(lang == 1){
  lang1 <- "ja, en"
} else {
if(lang == 2){
  lang1 <- "ja"
} else {
  lang1 <- "en"
}}

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

Prop <- Property
if(GroupBy){
  GroupBy00 <- paste('distinct ', Count , ' (count(', Count, ') as ?Count)', sep="")
  GroupBy01 <- paste('GROUP BY ', Count , sep="")
}else{
  GroupBy00 <- paste('(count(distinct ', Count, ') as ?Count)', sep="")
  GroupBy01 <- ""
}

Query <-paste('
SELECT ', GroupBy00, '
WHERE { ',
QID, ' ', Prop , ' ', Object, '.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'} ', GroupBy01, sep="")

#message(paste("Query: ", QID, sep=""))
suppressMessages(try(SPA <- data.frame(WikidataQueryServiceR::query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(GroupBy){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}else{}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
return(data.frame(SPA, stringsAsFactors = F))
}
