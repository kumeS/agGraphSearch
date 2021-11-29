if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("franc")){install.packages("franc")}; library(franc)

#Example
#data science (wd:Q2374463)
searchLOD_Wikidata(EntityLabel = "data science")
#dendritic cell (wd:Q506253)
searchLOD_Wikidata(EntityLabel = "dendritic cell")
#Kupffer cell (wd:Q513486)
searchLOD_Wikidata(EntityLabel = "Kupffer cell")
#hepatic macrophage
searchLOD_Wikidata(EntityLabel = "hepatic macrophage")
#acute leukemia (wd:Q976388)
searchLOD_Wikidata(EntityLabel = "acute leukemia")
#Stem cell Leukemia
searchLOD_Wikidata(EntityLabel = "Stem cell Leukemia")
#fern (Q80005)
searchLOD_Wikidata(EntityLabel = "fern")


#EntityLabel = "data science"; language=1; lang_option="en"; Message=F
searchLOD_Wikidata <- function(EntityLabel, language=1, lang_option="en", Message=F, lowercase=F){

Label.c <- LabelCount(EntityLabel=EntityLabel, language=language, lang_option=lang_option, Message=Message)
Alt.c <- AltLabelCount(EntityLabel=EntityLabel, language=language, lang_option=lang_option, Message=Message)
searchLabel.s <- searchLabel(EntityLabel=EntityLabel, language=language, lang_option=lang_option, Message=Message)
searchAltLabel.s <- searchAltLabel(EntityLabel=EntityLabel, language=language, lang_option=lang_option, Message=Message)

Dat <- c()
if(Label.c$count > 0){
Dat <- Dat %>% rbind(searchLabel.s)
}
if(Alt.c$count > 0){
Dat <- Dat %>% rbind(searchAltLabel.s)
}

if(lowercase){
if(all(c(Label.c$count == 0, grepl("[A-Z]", EntityLabel)))){
EntityLabel.t <-  tolower(EntityLabel)
Label.t <- LabelCount(EntityLabel=EntityLabel.t, language=language, lang_option=lang_option, Message=Message)
searchLabel.t <- searchLabel(EntityLabel=EntityLabel, language=language, lang_option=lang_option, Message=Message)
if(Label.t$count > 0){
Dat <- Dat %>% rbind(searchLabel.t)
}}

if(all(c(Alt.c$count == 0, grepl("[A-Z]", EntityLabel)))){
EntityLabel.t <-  tolower(EntityLabel)
Alt.t <- AltLabelCount(EntityLabel=EntityLabel.t, language=language, lang_option=lang_option, Message=Message)
searchAltLabel.t <- searchAltLabel(EntityLabel=EntityLabel.t, language=language, lang_option=lang_option, Message=Message)
if(Alt.t$count > 0){
Dat <- Dat %>% rbind(searchAltLabel.t)
}}
}

if(!is.null(Dat)){
  return(data.frame(Dat, stringsAsFactors = F, row.names=1:nrow(Dat)))
}else{
  return("No results")
}
}

#Counting representative labels
#EntityLabel = "data science"; language=1; lang_option="en"; Message=F
#EntityLabel
LabelCount <- function(EntityLabel, language=1, lang_option="en", Message=F){
LABEL <- EntityLabel
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

Query <-paste('
SELECT (count(distinct ?subject ) as ?Count )
WHERE
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

return(data.frame(label=as.character(LABEL),
                  property="rdfs:label",
                  count=as.numeric(SPA$Count),
                  stringsAsFactors = F))
}

AltLabelCount <- function(EntityLabel, language=1, lang_option="en", Message=F){
LABEL <- EntityLabel
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

Query <-paste('
SELECT (count(distinct ?subject ) as ?Count )
WHERE
{
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

return(data.frame(label=as.character(LABEL),
                  property="skos:altLabel",
                  count=as.numeric(SPA$Count),
                  stringsAsFactors = F))
}

#EntityLabel = "fern"; language=1; lang_option="en"; Message=F
#EntityLabel
searchLabel <- function(EntityLabel, language=1, lang_option="en", Message=F){
LABEL <- EntityLabel
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

Query <-paste('
SELECT distinct ?subject ?subjectLabel
WHERE
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '. ','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

if(nrow(SPA) == 0){ return("No results") }
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)

Lab <- grep(pattern="^wd:P", SPA$subject)
if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

return(data.frame(entity=c(unlist(SPA[,1])),
                  property="rdfs:label",
                  label=c(unlist(SPA[,2])),
                  stringsAsFactors = F, row.names=1:nrow(SPA)))
}


searchAltLabel <- function(EntityLabel, language=1, lang_option="en", Message=F){
LABEL <- EntityLabel
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

Query <-paste('
SELECT distinct ?subject ?subjectLabel
WHERE
{
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', rdfs.l, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

if(nrow(SPA) == 0){ return("No results") }
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)

Lab <- grep(pattern="^wd:P", SPA$subject)
if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

return(data.frame(entity=c(unlist(SPA[,1])),
                  property="skos:altLabel",
                  label=c(unlist(LABEL)),
                  stringsAsFactors = F, row.names=1:nrow(SPA)))
}









