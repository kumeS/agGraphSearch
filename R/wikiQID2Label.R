##' @title convert QID to Label via SPARQL query
##'
##' @param EntityQID a character of QID with wd:.
##' @param lang a numeric: 1: jpn, 2, en.
##' @param Message a logical
##'
##' @description This function convert QID to Label via SPARQL query.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export wikiQID2Label
##' @importFrom WikidataQueryServiceR query_wikidata(paste
##'

wikiQID2Label <- function(EntityQID, lang=1, Message=F){
if(is.null(EntityQID)){return(message("Warning: Not proper value of File_path"))}
#EntityQID="wd:Q99527517"

QID <- EntityQID
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }

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

Query <- paste0('
SELECT distinct ?entityNamej ?entityNamee
WHERE {
optional {', QID, ' rdfs:label ?entityNamej. filter(LANG(?entityNamej) = "ja")}
optional {', QID, ' rdfs:label ?entityNamee. filter(LANG(?entityNamee) = "en")}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}')

if(Message){message(paste("Query: ", QID, sep=""))}
suppressMessages(try(SPA <- WikidataQueryServiceR::query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(!exists("SPA")){return(message("Perhaps No Internet Services"))}
SPA$entityNamej[is.na(SPA$entityNamej)] <- SPA$entityNamee[is.na(SPA$entityNamej)]

return(data.frame(SPA, stringsAsFactors = F))

}
