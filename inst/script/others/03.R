if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("franc")){install.packages("franc")}; library(franc)


#EntityID="wd:Q2374463"; language=1; lang_option="en"

wikiCount <- function(EntityID="wd:Q2374463", language=1, lang_option="en", Count=T){

LABEL <- EntityID
if(language == 1){ rdfs.l <- "en" } else { if(language == 2){ rdfs.l <- lang_option } else { rdfs.l <- "en" } }

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

if(Count){
Query <-paste0('
SELECT distinct ?property ?propertyLabel (COUNT(?property) AS ?Count)
WHERE { ',LABEL, ' ?p ?o .
?property wikibase:directClaim ?p .
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }
} GROUP BY ?property ?propertyLabel ORDER BY DESC(?Count)')

suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{return(message("Perhaps No Internet Services"))}

return(data.frame(EntityID=as.character(LABEL),
                  SPA,
                  stringsAsFactors = F))
}else{
Query <-paste0('
SELECT distinct ?property ?propertyLabel ?o ?oLabel
WHERE { ',LABEL, ' ?p ?o .
?property wikibase:directClaim ?p .
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }
} ')

suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
  if(exists("SPA")){}else{return(message("Perhaps No Internet Services"))}

  return(data.frame(EntityID=as.character(LABEL),
                    SPA,
                    stringsAsFactors = F))


}
}

