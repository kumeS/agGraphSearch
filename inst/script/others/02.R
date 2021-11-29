if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("franc")){install.packages("franc")}; library(franc)


#QID => literal


wikiWD_QIDtoLabel <- function(EntityQID="wd:Q30060700", lang=1){
LABEL <- EntityQID
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
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

Query <-paste('
SELECT distinct ?entityNamej ?entityNamee
WHERE {
optional {', LABEL, ' rdfs:label ?entityNamej. filter(LANG(?entityNamej) = "ja")}
optional {', LABEL, ' rdfs:label ?entityNamee. filter(LANG(?entityNamee) = "en")}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

return(data.frame(SPA, stringsAsFactors = F))
}



wikiCount_QID_Num <- function(QID_Name="wd:Q2374463", lang=1, Message=F){
LABEL <- QID_Name

if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }
#SPA01 <- NA; SPA02 <- NA; SPA03A <- NA; SPA03B <- NA
#SPA04A <- NA; SPA04B <- NA; SPA05A <- NA; SPA05B <- NA
#SPA06A <- NA; SPA05B <- NA

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

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass)
WHERE { ',
LABEL, ' wdt:P279 ?parentClass.
}', sep="")
suppressMessages(try(SPA03A <- query_wikidata(paste(Prefix, Query03A), format = "simple"), silent = T))
if(exists("SPA03A")){}else{
  return(message("Perhaps No Internet Services"))}

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass)
WHERE {
?childClass wdt:P279 ', LABEL, '.
}', sep="")
suppressMessages(try(SPA04A <- query_wikidata(paste(Prefix, Query04A), format = "simple"), silent = T))

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance)
WHERE {
?instance wdt:P31 ', LABEL, '.
}', sep="")
suppressMessages(try(SPA05A <- query_wikidata(paste(Prefix, Query05A), format = "simple"), silent = T))

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf)
WHERE {',
LABEL, ' wdt:P31 ?instance.
}', sep="")
suppressMessages(try(SPA06A <- query_wikidata(paste(Prefix, Query06A), format = "simple"), silent = T))

SPA03A.SPA04A <- as.numeric(SPA03A) + as.numeric(SPA04A)
SPA06A.SPA05A <- as.numeric(SPA06A) + as.numeric(SPA05A)

SPA03A.SPA06A <- as.numeric(SPA03A) + as.numeric(SPA06A)
SPA04A.SPA05A <- as.numeric(SPA04A) + as.numeric(SPA05A)

SPA <- data.frame(LABEL=LABEL,
                  Count_Of_P279_P31=SPA03A.SPA04A+SPA06A.SPA05A,
                  Count_Of_P279_P31_up=SPA03A.SPA06A,
                  Count_Of_P279_P31_down=SPA04A.SPA05A,
                  Count_Of_P279=SPA03A.SPA04A,
                  Count_Of_P31=SPA06A.SPA05A,
                  SPA03A,
                  SPA04A,
                  SPA06A,
                  SPA05A,
                  stringsAsFactors=F)

return(data.frame(SPA, stringsAsFactors = F))
}

wikiWD_LabelAlt2QIDLabel <- function(EntityName="データサイエンス", AltLabel=F, lang=1, Message=F){

LABEL <- EntityName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
if(lang == 1){
    lang1 <- "ja, en"
    Select <- "?alsoKnownAsj ?alsoKnownAse"
    Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
    Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'
} else {
    if(lang == 2){
    lang1 <- "ja"
    Select <- "?alsoKnownAsj"
    Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
    Where2 <- ""
} else {
    lang1 <- "en"
    Select <- "?alsoKnownAse"
    Where1 <- ""
    Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'
  }
}

if(AltLabel){}else{
    Select <- " "
    Where1 <- ''
    Where2 <- ''
}

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
SELECT distinct ?subject ?subjectLabel ', Select, '
WHERE {
optional{?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.}
optional{?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.}',
Where1,
Where2,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
try(Lab <- grep(pattern="^wd:P", SPA$subject), silent = T)

if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

return(data.frame(SPA, stringsAsFactors = F))
}


