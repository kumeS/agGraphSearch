######################################################################################
### Description: Explore the upper graph structures of subClassOf Class from WikiData.
### Usage ###
## 1. Load the R file
## source("[Raw data URL]")
######################################################################################
## 2. get the subClassOf graph from a class.
## > GraphData <- wikiGraph(ClassName="データサイエンス", Upper=TRUE, lang=TRUE, Depth=8)
## or 
## > GraphData <- wikiGraph()
## Arguments: 
## ClassName: String
## Upper: if TRUE, Search the upper classes. if FALSE, Search the lower classes
## lang: if TRUE, reseach Japanese and English terms. if FALSE, reseach English terms.
## Depth: Number of graph layers
## notIN: Exclude from SPARQL search. Defined as "wd:XXXXX" class.
######################################################################################
## 3. create the network of subClassOf Graph.
## > WikiNetwork3d(GraphData)
## > WikiNetwork3d(SPAresults)
#### Other case
## > WikiVisNetwork(GraphData)
## Arguments: 
## GraphData: Output result of wikiGraph
## SPAresults: Output result of wikiGraph on the way
## 4. create the table of graph data.
## > GraphDT(Data=GraphData)
## > GraphForm(Data=GraphData)
## 5. create the data tree of graph data.
## > treeGraph(Data=GraphData)
######################################################################################

if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
#if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("readr")){install.packages("readr")}; library(readr)



################################################
#Option
################################################

options(max.print=999999)

################################################
###  Search wikidata ID
## only match ID
################################################
wikiWD <- function(ClassName="データサイエンス", lang=1){
LABEL <- ClassName
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
SELECT distinct ?subject ?subjectLabel
WHERE 
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)

Lab <- grep(pattern="^wd:P", SPA$subject)
if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

return(data.frame(SPA, stringsAsFactors = F))
}


##QID to Label
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

##########################################################################
##########################################################################
#ClassName="田中"; Property="wdt:P31"; Object="wd:Q101352"; lang=1
wikiCount_Label_Property_Object_v01 <- function(LabelName="田中", Property="wdt:P31", Object="wd:Q16521", lang=1){
LABEL <- LabelName
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

Prop <- Property

Query <-paste('
SELECT (count(distinct ?subject) as ?Count)
WHERE { 
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
?subject ', Prop , ' ', Object, '. 
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

#message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", LABEL, sep="")))}
return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################################
################################################################################################
wikiCount_QID_Property_Object_REGEX_v01 <- function(WDname="wd:Q12159869", Object="wd:Q101352", 
                                              Property="?p", Count="?p", lang=1,
                                              REGEX="^http://www.wikidata.org/prop/direct/P31"){
QID <- WDname
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

Prop <- Property

if(!(is.integer(grep("^http://", as.character(REGEX))) && length(grep("^http://", as.character(REGEX))) == 0L)){
  REGEX00 <- paste('Filter(regex(str(', Count, '), "', REGEX, '"))', sep="")
  REGEX01 <- paste('GROUP BY ', Count, sep="")
}else{
  REGEX00 <- ""
  REGEX01 <- ""
}

Query <-paste('
SELECT distinct ', Count , '(count(', Count, ') as ?Count)
WHERE { ', 
QID, ' ', Prop , ' ', Object, '. 
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
REGEX00,
'}', REGEX01, sep="")

#message(paste("Query: ", QID, sep=""))
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)

if(dim(SPA)[1] == 0){SPA[1,] <- NA}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
return(data.frame(SPA, stringsAsFactors = F))
}
################################################################################################
################################################################################################
#WDname="wd:Q12159869"; Property="?p"; Object="wd:Q101352"; Count="?p"; lang=1
wikiCount_QID_Property_Object_v02 <- function(WDname="wd:Q12159869", Object="wd:Q101352", 
                                              Property="?p", Count="?p", lang=1,
                                              GroupBy=FALSE){
QID <- WDname
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
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(GroupBy){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}else{}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
return(data.frame(SPA, stringsAsFactors = F))
}
##########################################################################
##########################################################################

wikiWD_R <- function(ClassName="データサイエンス"){
LABEL <- ClassName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
lang1 <- "ja, en"
  
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
SELECT distinct ?subject ?subjectjLabel ?subjecteLabel
WHERE 
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
optional{ ?subject rdfs:label ?subjectj .  filter(LANG(?subjectj) = "ja") }
optional{ ?subject rdfs:label ?subjecte .  filter(LANG(?subjecte) = "en") }
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")
  
message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
return(data.frame(SPA, stringsAsFactors = F))
}

#ラベルヒット、別名ヒット、インスタンスヒット、クラス関係ヒットとかも追加
wikiCount_Label_Num <- function(ClassName="データサイエンス", lang=1, Message=T){
LABEL <- ClassName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
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

Query01 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_Label)
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

suppressMessages(try(SPA01 <- query_wikidata(paste(Prefix, Query01), format = "simple"), silent = T))
if(exists("SPA01")){}else{
  return(message("Perhaps No Internet Services"))}

Query02 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_AltLabel)
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA02 <- query_wikidata(paste(Prefix, Query02), format = "simple"), silent = T))

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.
?subject wdt:P279 ?parentClass.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA03A <- query_wikidata(paste(Prefix, Query03A), format = "simple"), silent = T))

Query03B <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_altLabel)
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.
?subject wdt:P279 ?parentClass.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA03B <- query_wikidata(paste(Prefix, Query03B), format = "simple"), silent = T))

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.
?childClass wdt:P279 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA04A <- query_wikidata(paste(Prefix, Query04A), format = "simple"), silent = T))

Query04B <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_altLabel)
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.
?childClass wdt:P279 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA04B <- query_wikidata(paste(Prefix, Query04B), format = "simple"), silent = T))

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.
?instance wdt:P31 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA05A <- query_wikidata(paste(Prefix, Query05A), format = "simple"), silent = T))

Query05B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_altLabel)
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.
?instance wdt:P31 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA05B <- query_wikidata(paste(Prefix, Query05B), format = "simple"), silent = T))

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.
?subject wdt:P31 ?instance.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA06A <- query_wikidata(paste(Prefix, Query06A), format = "simple"), silent = T))

Query06B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_altLabel)
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.
?subject wdt:P31 ?instance.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA06B <- query_wikidata(paste(Prefix, Query06B), format = "simple"), silent = T))

SPA01.SPA02 <- as.numeric(SPA01) + as.numeric(SPA02)
SPA03A.SPA03B <- as.numeric(SPA03A) + as.numeric(SPA03B)
SPA04A.SPA04B <- as.numeric(SPA04A) + as.numeric(SPA04B)
SPA03A.SPA03B.SPA04A.SPA04B <- SPA03A.SPA03B + SPA04A.SPA04B
SPA06A.SPA06B <- as.numeric(SPA06A) + as.numeric(SPA06B)
SPA05A.SPA05B <- as.numeric(SPA05A) + as.numeric(SPA05B)
SPA05A.SPA05B.SPA06A.SPA06B <- SPA05A.SPA05B + SPA06A.SPA06B
SPA_Hit_ParentClass_InstanceOf <- SPA03A.SPA03B + SPA06A.SPA06B

SPA <- data.frame(LABEL=LABEL,
                  Hit_Label=SPA01.SPA02, 
                  Hit_ParentClass_InstanceOf=SPA_Hit_ParentClass_InstanceOf,
                  Hit_subClassOf=SPA03A.SPA03B.SPA04A.SPA04B,
                  Hit_Instance=SPA05A.SPA05B.SPA06A.SPA06B,
                  Hit_ParentClass=SPA03A.SPA03B,
                  Hit_ChildClass=SPA04A.SPA04B,
                  Hit_InstanceOf=SPA06A.SPA06B,
                  Hit_Has_Instance=SPA05A.SPA05B,
                  SPA01, SPA02, 
                  SPA03A, SPA03B, 
                  SPA04A, SPA04B, 
                  SPA06A, SPA06B, 
                  SPA05A, SPA05B,
                  stringsAsFactors=F)

return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################
################################################################################


################################################################################
################################################################################

################################################################################
#QID_Name="wd:Q2374463"; lang=1; Message=T
################################################################################

wikiCount_QID_Num <- function(QID_Name="wd:Q2374463", lang=1, Message00=T, Message=F){
LABEL <- QID_Name
if(Message00){message(LABEL)}

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


#クラス名と一致したクラスの別名検索
wikiWD_Alt <- function(ClassName="データサイエンス", lang=1){
LABEL <- ClassName
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
SELECT distinct ?subject ?subjectLabel ', Select,' 
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
Where1,
Where2,
'}', sep="")

message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
return(data.frame(SPA, stringsAsFactors = F))
}

##ClassNameへのインスタンス関係
wikiWD_Ins <- function(ClassName="データサイエンス", lang=1){
LABEL <- ClassName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
if(lang == 1){ 
    lang1 <- "ja, en"
    Select <- "?instanceOfjjLabel ?instanceOfeeLabel"
    Where1 <- 'optional {?instanceOfj wdt:P31 ?subject . }
               optional {?instanceOfj rdfs:label ?instanceOfjj . filter(LANG(?instanceOfjj) = "ja") }'
    Where2 <- 'optional {?instanceOfe wdt:P31 ?subject . } 
               optional {?instanceOfe rdfs:label ?instanceOfee . filter(LANG(?instanceOfee) = "en") }'
  } else { 
    if(lang == 2){
      lang1 <- "ja"
      Select <- "?instanceOfjjLabel"
      Where1 <- 'optional{ ?instanceOfj wdt:P31 ?subject . }
                 optional {?instanceOfj rdfs:label ?instanceOfjj . filter(LANG(?instanceOfjj) = "ja") }'
      Where2 <- ""
    } else {
      lang1 <- "en"
      Select <- "?instanceOfeeLabel"
      Where1 <- ""
      Where2 <- 'optional {?instanceOfe wdt:P31 ?subject . } 
                 optional {?instanceOfe rdfs:label ?instanceOfee . filter(LANG(?instanceOfee) = "en") }'
    }
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
SELECT distinct ?subject ?subjectLabel ', Select,' 
WHERE {
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
Where1,
Where2,
'}', sep="")
  
message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)

return(data.frame(SPA, stringsAsFactors = F))
}

################################################
###  Search wikidata ID
## Search the alternative name
################################################
#別名でヒットしたクラスのクラス名と別名を表示する
#クラス名と別名のoptional検索はtime comsuming

#ClassName <- "CU化学式"; lang=1

wikiWD2_Alt <- function(ClassName="データサイエンス", lang=1){
LABEL <- ClassName
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
  
message(paste("Query: ", LABEL, sep=""))
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

### New
wikiWD3_Alt <- function(ClassName="データサイエンス", AltLabel=F, lang=1, Message=F){
LABEL <- ClassName
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


################################################
##wikiWD_R + wikiWD2_Alt
################################################
wikiWD3 <- function(ClassName="データサイエンス"){
LABEL <- ClassName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
lang1 <- "ja, en"
  
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
SELECT distinct ?subject ?subjectjLabel ?subjecteLabel
WHERE 
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
optional{ ?subject rdfs:label ?subjectj .  filter(LANG(?subjectj) = "ja") }
optional{ ?subject rdfs:label ?subjecte .  filter(LANG(?subjecte) = "en") }
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA1 <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA1")){}else{
  return(message("Perhaps No Internet Services"))}

colnames(SPA1) <- c("subject", "Labelj","Labele")

Select <- "?alsoKnownAsj ?alsoKnownAse"
Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'

Query <-paste('
SELECT distinct ?subject ', Select, '
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.',
Where1,
Where2,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")

suppressMessages(try(SPA2 <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
colnames(SPA2) <- c("subject", "Labelj","Labele")
SPA3 <- rbind(SPA1, SPA2)

try(SPA3$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA3$subject), silent = T)
return(data.frame(SPA3, stringsAsFactors = F))
}

###########################################################
##wikiWD_R + wikiWD2_Alt かつ subClassOf関係ありのもの 
###########################################################
wikiWD4 <- function(ClassName="データサイエンス"){
LABEL <- ClassName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
lang1 <- "ja, en"
  
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
SELECT distinct ?subject ?subjectjLabel ?subjecteLabel
WHERE 
{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
optional{ ?subject rdfs:label ?subjectj .  filter(LANG(?subjectj) = "ja") }
optional{ ?subject rdfs:label ?subjecte .  filter(LANG(?subjecte) = "en") }
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")
  
message(paste("Query: ", LABEL, sep=""))
suppressMessages(try(SPA1 <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(exists("SPA1")){}else{
  return(message("Perhaps No Internet Services"))}
colnames(SPA1) <- c("subject", "Labelj","Labele")
  
Select <- "?alsoKnownAsj ?alsoKnownAse"
Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'
  
Query <-paste('
SELECT distinct ?subject ', Select, '
WHERE {
?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.',
Where1,
Where2,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")
  
suppressMessages(try(SPA2 <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
colnames(SPA2) <- c("subject", "Labelj","Labele")
SPA3 <- rbind(SPA1, SPA2)
  
try(SPA3$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA3$subject), silent = T)
return(data.frame(SPA3, stringsAsFactors = F))
}

################################################
###  Search Graph wdt:P279 only
################################################
#ラベル複数一致してもそれぞれで検索
#ラベルが一致してもsubClassOfがなければ除く
wikiGraph <- function(ClassName="データサイエンス", Upper=TRUE, lang=1, Depth=5, 
                      notIN=NULL, output=FALSE, FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), HalfWay=FALSE, ...){

  LABEL <- ClassName
  SPA2 <- NULL
  SPAresults <<- NULL

  x <- 0
  y <- 0
  if(Upper){ 
    SearchQuery01 <- "?parentClass ?parentClassLabel"
    SearchQuery02 <- "?subject wdt:P279 ?parentClass."
  } else {
    SearchQuery01 <- "?childClass ?childClassLabel"
    SearchQuery02 <- "?childClass wdt:P279 ?subject."
  }
  if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

  if(is.null(notIN)){ 
    notIN01 <- "" 
  } else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

repeat {
SPA1 <- NULL
x <- x + 1

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

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE 
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
message(paste("Query: ", LABELn, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}

if(is.null(SPA1) | Depth == x) { 
  cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ break }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3); break } else {  }
if(Upper){ LABEL <- unique(SPA1$parentClassLabel) } else { LABEL <- unique(SPA1$childClassLabel) }
}}


##Excludeクラスの１つ先まで検索
wikiGraph_Exc <- function(ClassName="データサイエンス", Upper=TRUE, lang=TRUE, Depth=5, 
                        Exclude=NULL, output=FALSE, FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), HalfWay=FALSE, ...){
    
    LABEL <- ClassName
    SPA2 <- NULL
    SPAresults <<- NULL
    
    x <- 0
    y <- 0
    if(Upper){ 
      SearchQuery01 <- "?parentClass ?parentClassLabel"
      SearchQuery02 <- "?subject wdt:P279 ?parentClass."
    } else {
      SearchQuery01 <- "?childClass ?childClassLabel"
      SearchQuery02 <- "?childClass wdt:P279 ?subject."
    }
    if(lang){ lang1 <- "ja, en" } else { lang1 <- "ja" }

    repeat {
      SPA1 <- NULL
      x <- x + 1
      
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
      
for(n in seq_len(length(LABEL))){
  # n <- 1
  LABELn <- LABEL[n]
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
        
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE 
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")
        
  y <- y + 1
  message(paste("Query: ", LABELn, sep=""))
  suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
  if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
}

  if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
  if(HalfWay){SPA2 ->> SPAresults}
  
  if(is.null(SPA1) | Depth == x) { 
    cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
    if(Upper){
      SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
    }else{
      SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
    }
    SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),]
    if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}
    return(SPA3); break } else {  }

  if(Upper){ 
    for(m in seq_len(length(Exclude))){
      SPA1 <- SPA1[SPA1$subject != Exclude[m],]
    }
    LABEL <- unique(SPA1$parentClassLabel) 
  } else {
    for(m in seq_len(length(Exclude))){
      SPA1 <- SPA1[SPA1$subject != Exclude[m],]
    }
    LABEL <- unique(SPA1$childClassLabel) 
}}}

################################################################################################
################################################################################################
wikiGraph_plus <- function(ClassName="データサイエンス", Depth=5, 
                           Upper=TRUE, lang=1, notIN=NULL, output=FALSE, 
                           FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                           HalfWay=FALSE, ...){

message("\nwikiGraph_plus start !! ")
LABEL <- ClassName
SPA2 <- NULL
SPAresults <<- NULL
      
x <- 0
y <- 0
if(Upper){ 
  SearchQuery01 <- "?parentClass ?parentClassLabel"
  SearchQuery02 <- "?subject wdt:P279 ?parentClass."
} else {
  SearchQuery01 <- "?childClass ?childClassLabel"
　SearchQuery02 <- "?childClass wdt:P279 ?subject."
}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}
      
if(is.null(notIN)){ 
notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

SPA1 <- NULL
x <- x + 1

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

LABELn <- LABEL
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE 
{
optional { ?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.}
optional { ?subject skos:altLabel \"', LABELn, '\"@', rdfs.l, '.}',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
message(paste("Query: ", LABELn, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}

if(is.null(SPA1) | Depth == x) { 
  cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3); suppressMessages(try(stop(), silent = T)) } else {  }

if(Upper){ 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
} else { 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
}

repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- LABEL02

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]
 
if(Upper){ 
  SearchQuery03 <- paste(LABELn, " wdt:P279 ?parentClass.", sep="")
} else {
  SearchQuery03 <- paste("?childClass wdt:P279 ", LABELn, ".", sep="")
}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE
{', 
SearchQuery03,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")
          
y <- y + 1
message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- LABELn, silent = T)
try(SPA$subjectLabel <- LABELnL, silent = T)

if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
  
if(is.null(SPA1) | Depth == x) { 
message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ",sep=""))
if(Upper){
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
}else{
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
}

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ break }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
return(SPA3); break } else {  }

if(Upper){
  SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
  SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$parentClass 
  LABEL02 <- SPA1$parentClassLabel
}else{
  SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
  SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$childClass
  LABEL02 <- SPA1$childClassLabel
}}}

################################################################################################
################################################################################################
wikiGraph_plus_rdfs <- function(ClassName="データサイエンス", Depth=5, 
                           Upper=TRUE, lang=1, notIN=NULL, output=FALSE, 
                           FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                           HalfWay=F, ...){

#message("\n wikiGraph_plus start !! ")
LABEL <- ClassName
SPA2 <- NULL
SPAresults <<- NULL
  
x <- 0
y <- 0
if(Upper){ 
  SearchQuery01 <- "?parentClass ?parentClassLabel"
  SearchQuery02 <- "?subject wdt:P279 ?parentClass."
} else {
  SearchQuery01 <- "?childClass ?childClassLabel"
  SearchQuery02 <- "?childClass wdt:P279 ?subject."
}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

if(is.null(notIN)){ 
  notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

SPA1 <- NULL
x <- x + 1

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
  
LABELn <- LABEL
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
  
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE {
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
#message(paste("Query: ", LABELn, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
  
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
  
if(is.null(SPA1) | Depth == x) { 
  cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3); suppressMessages(try(stop(), silent = T)) } else {  }
  
if(Upper){ 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
} else { 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
}
  
repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- LABEL02
    
for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]
    
if(Upper){ 
  SearchQuery03 <- paste(LABELn, " wdt:P279 ?parentClass.", sep="")
} else {
  SearchQuery03 <- paste("?childClass wdt:P279 ", LABELn, ".", sep="")
}
      
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE{', 
SearchQuery03,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")
      
y <- y + 1
#message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- LABELn, silent = T)
try(SPA$subjectLabel <- LABELnL, silent = T)
      
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
      
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
    
if(is.null(SPA1) | Depth == x) { 
  #message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ",sep=""))
  if(Upper){
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ return("NULL") }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
return(SPA3) } else {  }
    
if(Upper){
  SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
　SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$parentClass 
  LABEL02 <- SPA1$parentClassLabel
}else{
  SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
  SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$childClass
  LABEL02 <- SPA1$childClassLabel
}}}



################################################
###  Search wdt:P279, wdt:P31
################################################
#SearchQuery01 <- "?parentClass ?parentClassLabel ?property ?propertyLabel"
#SearchQuery02 <- "?subject wdt:P279 ?parentClass. ?subject ?prop ?parentClass."
#SearchQuery03 <- "?subject wdt:P31 ?parentClass. ?subject ?prop ?parentClass."

#ClassName=Wiki.Label[1]
#ClassName="ポリビニルアルコール"
#Depth=5;Upper=TRUE; lang=1; notIN=NULL; output=FALSE;FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""); HalfWay=F

wikiGraph_plus_rdfs_v2 <- function(ClassName="データサイエンス", Depth=999, 
                           Upper=TRUE, lang=1, notIN=NULL, output=FALSE, 
                           FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                           HalfWay=F, ...){

#message("\n wikiGraph_plus start !! ")
LABEL <- ClassName
SPA2 <- NULL
SPAresults <<- NULL
  
x <- 0
y <- 0
if(Upper){ 
  SearchQuery01 <- "?parentClass ?parentClassLabel"
  SearchQuery02 <- "?subject wdt:P31|wdt:P279 ?parentClass."
} else {
  SearchQuery01 <- "?childClass ?childClassLabel"
  SearchQuery02 <- "?childClass wdt:P31|wdt:P279 ?subject."
}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

if(is.null(notIN)){ 
  notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

SPA1 <- NULL
x <- x + 1

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
  
LABELn <- LABEL
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
  
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE {
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
#message(paste("Query: ", LABELn, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
  
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
  
if(is.null(SPA1) | Depth == x) { 
  cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3); suppressMessages(try(stop(), silent = T)) } else {  }
  
if(Upper){ 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
} else { 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
}
  
repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- LABEL02
    
for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]
    
if(Upper){ 
  SearchQuery03 <- paste(LABELn, " wdt:P279 ?parentClass.", sep="")
} else {
  SearchQuery03 <- paste("?childClass wdt:P279 ", LABELn, ".", sep="")
}
      
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE{', 
SearchQuery03,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")
      
y <- y + 1
#message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- LABELn, silent = T)
try(SPA$subjectLabel <- LABELnL, silent = T)
      
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
      
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
    
if(is.null(SPA1) | Depth == x) { 
  #message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ",sep=""))
  if(Upper){
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ return("NULL") }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
return(SPA3) } else {  }
    
if(Upper){
  SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
　SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$parentClass 
  LABEL02 <- SPA1$parentClassLabel
}else{
  SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
  SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$childClass
  LABEL02 <- SPA1$childClassLabel
}}}

#################################################################################


################################################################################################
################################################################################################
#ClassName="データサイエンス"; Depth=5; Property=TRUE; Prop1="wdt:P279"; Prop2="wdt:P279"; Upper=TRUE; lang=1; notIN=NULL; output=FALSE; FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""); HalfWay=F
wikiGraph_plus_rdfs_v3 <- function(ClassName="データサイエンス", Depth=5,
                           PropertyView=TRUE, Prop1="wdt:P279", Prop2="wdt:P279", 
                           Upper=TRUE, lang=1, notIN=NULL, output=FALSE, 
                           FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                           HalfWay=F, ...){
#message("\n wikiGraph_plus start !! ")
LABEL <- ClassName
SPA2 <- NULL
SPAresults <<- NULL

x <- 0
y <- 0
if(Upper){
  SearchQuery01 <- "?parentClass ?parentClassLabel"
  SearchQuery02 <- paste("?subject ", Prop1, " ?parentClass.", sep="")
if(PropertyView){
  SearchProperty01 <- " ?property ?propertyLabel"
  SearchProperty02 <- "?subject ?prop ?parentClass."
  SearchProperty03 <- "?property wikibase:directClaim ?prop ."
}else{
  SearchProperty01 <- ""
  SearchProperty02 <- ""
  SearchProperty03 <- ""
}} else {
  SearchQuery01 <- "?childClass ?childClassLabel"
  SearchQuery02 <- paste("?childClass ", Prop1, " ?subject.", sep="")
if(PropertyView){
  SearchProperty01 <- " ?property ?propertyLabel"
  SearchProperty02 <- "?childClass ?prop ?subject."
  SearchProperty03 <- "?property wikibase:directClaim ?prop."
}else{
  SearchProperty01 <- ""
  SearchProperty02 <- ""
  SearchProperty03 <- ""
}
}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

if(is.null(notIN)){ 
  notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

SPA1 <- NULL
x <- x + 1

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
  
LABELn <- LABEL
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
  
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, SearchProperty01, '
WHERE {
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,
SearchProperty02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
SearchProperty03,
notIN01,
'}', sep="")

y <- y + 1
#message(paste("Query: ", LABELn, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
if(PropertyView){
  try(SPA$property <- gsub("http://www.wikidata.org/entity/", "wdt:", SPA$property), silent = T)
}else{}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
  
if(is.null(SPA1) | Depth == x){
  cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3) } else {  }
  
if(T){ 
  LABEL01 <- SPA1[,3:4]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
} else { }
  
repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- LABEL02

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]

if(Upper){ 
  SearchQuery03 <- paste(LABELn, " ", Prop2, " ?parentClass.", sep="")
if(PropertyView){
  SearchProperty02 <- paste(LABELn, " ?prop ?parentClass.", sep="")
}else{}
} else {
  SearchQuery03 <- paste("?childClass ", Prop2, " ", LABELn, ".", sep="")
if(PropertyView){
  SearchProperty02 <- paste("?childClass ?prop ", LABELn, ".", sep="")
}else{}
}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, SearchProperty01, '
WHERE{', 
SearchQuery03,
SearchProperty02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
SearchProperty03,
notIN01,
'}', sep="")
      
y <- y + 1
#message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))
try(SPA$subject <- LABELn, silent = T)
try(SPA$subjectLabel <- LABELnL, silent = T)
      
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}
if(PropertyView){
  try(SPA$property <- gsub("http://www.wikidata.org/entity/", "wdt:", SPA$property), silent = T)
}else{}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
    
if(is.null(SPA1) | Depth == x) { 
  #message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ",sep=""))
  if(Upper){
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
  SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ return("NULL") }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
return(SPA3) } else {  }
    
if(Upper){
  SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
　SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$parentClass 
  LABEL02 <- SPA1$parentClassLabel
}else{
  SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
  SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
  LABELg <- SPA1$childClass
  LABEL02 <- SPA1$childClassLabel
}}}
################################################################################################
################################################################################################
################################################################################################

wikiGraph_plusWD_v1 <- function(WD_ClassName="wd:Q2374463", Depth=1, 
                             Upper=FALSE, lang = 1, notIN=NULL, output=FALSE, 
                             FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                             HalfWay=F, ...){

#message("\n wikiGraph_plusWD start !! ")
LABELn <- WD_ClassName
#LABELn <- "wd:Q15841121"

SPA2 <- NULL
SPAresults <<- NULL
  
x <- 0
y <- 0

if(Upper){ 
  SearchQuery01 <- "?parentClass ?parentClassLabel "
  SearchQuery02 <- paste(LABELn, " wdt:P279 ?parentClass. ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
} else {
  SearchQuery01 <- "?childClass ?childClassLabel "
  SearchQuery02 <- paste("?childClass wdt:P279 ", LABELn, ". ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
}

if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

if(is.null(notIN)){ 
  notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}
  
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
SELECT distinct ?subject ?subjectLabelj ?subjectLabele
WHERE{',
'optional {', SearchQuery03,'} ',
'optional {', SearchQuery04,'}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

suppressMessages(try(SPA_nam <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))

SPA_nam$subject <- LABELn
a <- unlist(SPA_nam[,c(2:3)]); b <- a[!is.na(a)]
LABEL02 <- b[1]
LABELg <- LABELn

repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- as.character(LABEL02)

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]
      
if(Upper){
  SearchQuery01 <- "?parentClass ?parentClassLabel "
  SearchQuery02 <- paste(LABELn, " wdt:P279 ?parentClass. ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
} else {
  SearchQuery01 <- "?childClass ?childClassLabel "
  SearchQuery02 <- paste("?childClass wdt:P279 ", LABELn, ". ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ?subjectLabelj  ?subjectLabele ', SearchQuery01, '
WHERE{',
SearchQuery02,
'optional {', SearchQuery03,'} ',
'optional {', SearchQuery04,'}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
#message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"), silent = T))

try(SPA$subject <- LABELn, silent = T)
try(SPA$subjectLabel <- LABELnL, silent = T)
if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=as.character(LABELnL), Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
    
if(is.null(SPA1) | Depth == x) { 
#message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ", sep=""))
if(Upper){
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
}else{
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
}

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ return(print("No results")) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
SPA4 <- SPA3
return(SPA4) } else {}

if(Upper){
SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
LABELg <- SPA1$parentClass
LABEL02 <- SPA1$parentClassLabel
}else{
SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
LABELg <- SPA1$childClass
LABEL02 <- SPA1$childClassLabel}
}
}

################################################################################################
################################################################################################
#WD_EntityID="wd:Q101487"; Prop1="wdt:P31|wdt:P279"; Prop2="wdt:P279"
#Depth=99;Upper=T; lang = 1; notIN=NULL; output=FALSE; FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""); HalfWay=F

wikiGraph_plusWD_v2 <- function(WD_EntityID="wd:Q2374463", Depth=5, 
                                Prop1="wdt:P31|wdt:P279", Prop2="wdt:P279",
                                Upper=T, lang = 1, notIN=NULL, output=FALSE, 
                                FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                                HalfWay=F, ...){
#message("\n wikiGraph_plusWD_v2 start !! ")
LABELn <- WD_EntityID
SPA2 <- NULL
SPAresults <- NULL
  
x <- 0
y <- 0

if(Upper){ 
  SearchQuery01 <- "?parentClass ?parentClassLabel "
  SearchQuery02 <- paste(LABELn, " ", Prop1, " ?parentClass. ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
} else {
  SearchQuery01 <- "?childClass ?childClassLabel "
  SearchQuery02 <- paste("?childClass ", Prop1, " ", LABELn, ". ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
}

if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2) { lang1 <- "ja" } else { lang1 <- "en" }}

if(is.null(notIN)){ 
  notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

SPA1 <- NULL
x <- x + 1

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
SELECT distinct ?subject ?subjectLabel ?subjectLabelj ?subjectLabele', SearchQuery01, '
WHERE{',
SearchQuery02,
'optional {', SearchQuery03,'} ',
'optional {', SearchQuery04,'}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". } ',
notIN01,
'}', sep="")

#tail(SPA)
y <- y + 1
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple"), stringsAsFactors = F), silent = T))
SPA$subject <- LABELn

if(nrow(SPA) == 0){ }else{
if(any(is.na(SPA))){
SPA$subjectLabel[is.na(SPA$subjectLabel)] <- SPA$subjectLabelj[is.na(SPA$subjectLabel)]
SPA$subjectLabel[is.na(SPA$subjectLabel)] <- SPA$subjectLabele[is.na(SPA$subjectLabel)]

SPA$subjectLabelj[is.na(SPA$subjectLabelj)] <- SPA$subjectLabele[is.na(SPA$subjectLabelj)]
SPA$subjectLabele[is.na(SPA$subjectLabele)] <- SPA$subjectLabelj[is.na(SPA$subjectLabele)]
}else{}}

try(SPA$subject <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)

if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}

LABELnL01 <- unique(SPA$subjectLabel)[1]
if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=LABELnL01, Depth=x, stringsAsFactors = F)) -> SPA1}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}

if(is.null(SPA1) | Depth == x){
  #cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, sep=""))
  if(Upper){
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
  }else{
    SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
  }
  try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
  return(SPA3) } else {  }

if(T){
  LABEL01 <- SPA1[,5:6]
  LABELg  <- LABEL01[,1]
  LABEL02 <- LABEL01[,2]
} else { }

repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- as.character(LABEL02)

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]

if(Upper){
  SearchQuery01 <- "?parentClass ?parentClassLabel "
  SearchQuery02 <- paste(LABELn, " ", Prop2, " ?parentClass. ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
} else {
  SearchQuery01 <- "?childClass ?childClassLabel "
  SearchQuery02 <- paste("?childClass ", Prop2, " ", LABELn, ". ", sep="")
  SearchQuery03 <- paste(LABELn, " rdfs:label ?subjectLabelj. filter(LANG(?subjectLabelj) = \"ja\") ", sep="")
  SearchQuery04 <- paste(LABELn, " rdfs:label ?subjectLabele. filter(LANG(?subjectLabele) = \"en\") ", sep="")
}

Query <-paste('
SELECT distinct ?subject ?subjectLabel ?subjectLabelj  ?subjectLabele ', SearchQuery01, '
WHERE{',
SearchQuery02,
'optional {', SearchQuery03,'} ',
'optional {', SearchQuery04,'}
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
notIN01,
'}', sep="")

y <- y + 1
#message(paste("Query: ", LABELnL, sep=""))
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple"), stringsAsFactors = F), silent = T))

try(SPA$subject <- LABELn, silent = T)
if(nrow(SPA) == 0){ }else{
if(any(is.na(SPA))){
SPA$subjectLabel[is.na(SPA$subjectLabel)] <- SPA$subjectLabelj[is.na(SPA$subjectLabel)]
SPA$subjectLabel[is.na(SPA$subjectLabel)] <- SPA$subjectLabele[is.na(SPA$subjectLabel)]

SPA$subjectLabelj[is.na(SPA$subjectLabelj)] <- SPA$subjectLabele[is.na(SPA$subjectLabelj)]
SPA$subjectLabele[is.na(SPA$subjectLabele)] <- SPA$subjectLabelj[is.na(SPA$subjectLabele)]
}else{}}

if(Upper){
  try(SPA$parentClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$parentClass), silent = T)
}else{
  try(SPA$childClass <- gsub("http://www.wikidata.org/entity/", "wd:", SPA$childClass), silent = T)
}

if(nrow(SPA) == 0){ }else{SPA1 %>% rbind(data.frame(SPA, group=as.character(LABELnL), Depth=x, stringsAsFactors = F)) -> SPA1}
}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}

if(is.null(SPA1) | Depth == x) { 
#message(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n ", sep=""))
if(Upper){
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
}else{
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
}

if(nrow(SPA2) > 0){rownames(SPA2) <- 1:nrow(SPA2)}
try(SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),], silent=T)
if(nrow(SPA3) > 0){rownames(SPA3) <- 1:nrow(SPA3)}

if(!any(ls() == "SPA3")){ return(print("No results")) }else{  if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
SPA4 <- SPA3
return(SPA4) } else {}

if(Upper){
SPA1$Unique <- paste(SPA1$parentClass, ".", SPA1$parentClassLabel, sep="")
SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
LABELg <- SPA1$parentClass
LABEL02 <- SPA1$parentClassLabel
}else{
SPA1$Unique <- paste(SPA1$childClass, ".", SPA1$childClassLabel, sep="")
SPA1 <- SPA1[as.numeric(as.character(rownames(unique(SPA1['Unique'])))),]
LABELg <- SPA1$childClass
LABEL02 <- SPA1$childClassLabel}
}
}

################################################################################################
#Up
################################################################################################

wikiGraph_plusWD_v2_up <- function(WD_EntityID="wd:Q2374463", Depth=99, 
                                Prop1="wdt:P31|wdt:P279", Prop2="wdt:P279",
                                Upper=T, lang = 1, notIN=NULL, output=FALSE, 
                                FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                                HalfWay=F){
wikiGraph_plusWD_v2(WD_EntityID=WD_EntityID, Depth=Depth, 
                    Prop1=Prop1, Prop2=Prop2,
                    Upper=Upper, lang = lang, notIN=notIN, output=output, 
                    FileName=FileName, HalfWay=HalfWay)
}

wikiGraph_plusWD_v2_down <- function(WD_EntityID="wd:Q2374463", Depth=10, 
                                Prop1="wdt:P31|wdt:P279", Prop2="wdt:P279",
                                Upper=F, lang = 1, notIN=NULL, output=FALSE, 
                                FileName=paste("WikiGraph_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                                HalfWay=F){
Result <- wikiGraph_plusWD_v2(WD_EntityID=WD_EntityID, Depth=Depth, 
                    Prop1=Prop1, Prop2=Prop2,
                    Upper=Upper, lang = lang, notIN=notIN, output=output, 
                    FileName=FileName, HalfWay=HalfWay)
return(Result)
}


################################################################################################
################################################################################################
################################################################################################
wikiGraph2_Count <- function(ClassName="データサイエンス", Upper=TRUE, lang=TRUE, Depth=5, 
                       notIN=NULL, output=FALSE, FileName=paste("WikiGraph2_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), 
                       HalfWay=TRUE, InstanceLimit=100, ...){
  LABEL <- ClassName
  SPA2 <- NULL
  SPAd <- NULL
  SPAresults2 <<- NULL
  x <- 0
  y <- 0
  z <- 0
  if(Upper){ 
    SearchQuery00 <- "(Count(?parentClass) As ?Count)"
    SearchQuery01 <- "?parentClass ?parentClassLabel ?property ?propertyLabel"
    SearchQuery02 <- "?subject wdt:P279 ?parentClass. ?subject ?prop ?parentClass."
    #SearchQuery03 <- "?subject wdt:P31 ?parentClass. ?subject ?prop ?parentClass."
  } else {
    SearchQuery00 <- "(Count(?childClass) As ?Count)"
    SearchQuery01 <- "?childClass ?childClassLabel ?property ?propertyLabel"
    SearchQuery02 <- "?childClass wdt:P279 ?subject. ?childClass ?prop ?subject."
    #SearchQuery03 <- "?subject wdt:P31 ?childClass. ?subject ?prop ?childClass."
  }  

  if(lang){ lang1 <- "ja, en" } else { lang1 <- "ja" }
  
  if(is.null(notIN)){ 
    notIN01 <- "" 
  } else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}
  
repeat {
SPA1 <- NULL
SPAc <- NULL
x <- x + 1
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
for(n in seq_len(length(LABEL))){
LABELn <- LABEL[n]
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
      
Query <-paste('
SELECT ', SearchQuery00, '
WHERE 
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
?property wikibase:directClaim ?prop .
FILTER ( ?prop IN (wdt:P279) )',
notIN01,
'}', sep="")
      
y <- y + 1
#message("wdt:P279")
SPA <- query_wikidata(paste(Prefix, Query), format = "simple")
SPA <- data.frame(Subject=LABELn, Property="wdt:P279", SPA)

Query <-paste('
SELECT ', SearchQuery00, '
WHERE
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery03,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
?property wikibase:directClaim ?prop .
FILTER ( ?prop IN (wdt:P31) )',
notIN01,
'}', sep="")
#message("wdt:P31")
#LABELn -> AAAA; message(AAAA)
y <- y + 1
A <- try(SPA0 <- query_wikidata(paste(Prefix, Query), format = "simple"), silent=T)
if(class(A) == "try-error"){
  warning("SPARQL Error"); z <- z + 1
}else{
SPA0 <- data.frame(Subject=LABELn, Property="wdt:P31", SPA0)
SPA %>% rbind(SPA0) -> SPA
}

########################
## wikiGraph2と同じ
########################
Query <-paste('
SELECT distinct ?subject ?subjectLabel ', SearchQuery01, '
WHERE 
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
?property wikibase:directClaim ?prop .
FILTER ( ?prop IN (wdt:P279) )',
notIN01,
'}', sep="")

y <- y + 1
#message("wdt:P279")
SPAa <- query_wikidata(paste(Prefix, Query), format = "simple")

#suppressMessages(SPA <- query_wikidata(paste(Prefix, Query), format = "simple"))
if(nrow(SPA) == 0){ }else{ SPA1 %>% rbind(data.frame(SPA, Depth=x, stringsAsFactors = F)) -> SPA1}
if(nrow(SPAa) == 0){ }else{ SPAc %>% rbind(data.frame(SPAa, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPAc}
}
if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2; SPAd %>% rbind(SPAc) -> SPAd}
if(HalfWay){SPA2 ->> SPAresults3}
if(HalfWay){SPAd ->> SPAresults4}
#pb$tick()
if(is.null(SPA1) | Depth == x) {
cat(paste("\nFinished. \n Number of SPARQL Query: ", y, "\n Depth from the Query  : ", x, "\n Error number : ", z, sep=""))
if(output){ write.table(SPA2, file = FileName, sep = ",", row.names = F) }else{}
return(SPA2); break } else {  }
if(Upper){
LABEL <- unique(SPAc$parentClassLabel) 
}else{ 
LABEL <- unique(SPAc$childClassLabel) 
}}}
    
################################################
###  Create the network graphs
################################################
## simpleNetwork version
WikiNetwork3d <- function(Graph = GraphData, output=FALSE, file=paste("Wikidata_Network3d", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""),
                          linkDistance= 40, charge=-30, fontSize=6, opacity=0.8){
    Links <- data.frame(source= Graph[,2], target=Graph[,4], value=NA, stringsAsFactors = F)
    N3d <- simpleNetwork(Links,
                  linkDistance = linkDistance, charge = charge, fontSize = fontSize, fontFamily = "Arial",
                  linkColour = "#666", nodeColour = "#225c87", opacity = opacity, zoom = T) 
    if(output){
      N3d %>% htmlwidgets::saveWidget(file = file)
      browseURL(file)
    } else {}
    return(N3d)
}

#Graph=GraphData2; Count=2; Size=10; SmallSize=5; FontSize=7; StarSize=10; SEED=123; Selected=NULL; output=FALSE; file="Wikidat_visNet01.html"; HeightSclale = "750px"; WidthSclale = "110%"
  
## visNetwork version
WikiVisNetwork <- function(Graph=GraphData, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                           SEED=123, Selected=NULL, Browse=FALSE, output=FALSE, file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE){
  if(is.null(Graph)){return("NULL")}
  set.seed(SEED)
  #if(nrow(Graph) > 5000){message("Too huge data"); break()}
  if(!any(colnames(Graph) == "propertyLabel")){
    if(any(colnames(Graph) == "parentClassLabel")){
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$parentClassLabel, Graph$parentClassLabel), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel)
    } else { 
      LABEL <- c(Graph$subjectLabel, Graph$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$childClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$childClassLabel, to = Graph$subjectLabel)
    }
  } else {
    if(any(colnames(Graph) == "parentClassLabel")){
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", 
                          color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, 
                          stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel,
                          dashes = c(ifelse(Graph$property == "http://www.wikidata.org/entity/P31", TRUE, FALSE)))
      
      edges1 <- edges[,2:3]; colnames(edges1) <- c("id", "shape")
      edges2 <- edges1[as.numeric(as.character(rownames(unique(edges1['id'])))),]
      nodes1 <- merge(nodes, edges2, by="id", all = T, sort = F)
      nodes1$shape[is.na(nodes1$shape)] <- "FALSE"
      nodes1$shape <- ifelse(nodes1$shape, "star", "dot")
      nodes <- nodes1
    } else { 
      Graph1 <- Graph[Graph$property == "http://www.wikidata.org/entity/P279",] 
      LABEL <- c(Graph1$subjectLabel, Graph1$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph1$subjectLabel, Graph1$childClassLabel), group = c(Graph1$group, Graph1$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, shape="dot", stringsAsFactors = F)
      edges <- data.frame(from = Graph1$childClassLabel, to = Graph1$subjectLabel, dashes=FALSE)
      
      Graph2 <- Graph[Graph$property != "http://www.wikidata.org/entity/P279",]
      LABEL2 <- c(Graph2$childClassLabel)
      LABEL2 <- data.frame(id = unique(LABEL2), stringsAsFactors = F)
      Group2 <- data.frame(id = c(Graph2$childClassLabel), group = c(Graph2$group), stringsAsFactors = F)
      Group3 <- Group2[as.numeric(as.character(rownames(unique(Group2['id'])))),]
      LABEL2 <- merge(LABEL2, Group3, by="id", sort=F)
      nodes2 <- data.frame(id = LABEL2$id, label = LABEL2$id, group=LABEL2$group, color.background="lightyellow", color.border="goldenrod1", color.highlight="orangered", size=StarSize, font.size =FontSize, shape="star", stringsAsFactors = F)
      edges2 <- data.frame(from = Graph2$subjectLabel, to = Graph2$childClassLabel, dashes=TRUE)

      edges %>% rbind(edges2) -> edges
      }
  }
  
if(Count > 1){
    Count10 <- table(nodes$group)[order(table(nodes$group))]
    group2 <- nodes$group
    id2 <- nodes$id
    if(NodeColorRandom){
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    }else{
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))
    }
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        #n <- 241
        nodes$color.background[group2 == names(Count10)[n]] <- ColorSpa[n]
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
        nodes$size[group2 == names(Count10)[n]] <- Size
     }else{
        #nodes$size[group2 == names(Count10)[n]] <- SmallSize
        #nodes$group[group2 == names(Count10)[n]] <- "Others"
     }}
  }else{}
  
  if(any(colnames(Graph) == "parentClassLabel")){
    if(Count > 1){
      for(n in seq_len(length(Count10))){
        if(Count10[n] > Count){
          #n <- 241
          #head(nodes)
          nodes$color.background[id2 == names(Count10)[n]] <- ColorSpa[n]
          nodes$color.border[id2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
          nodes$size[id2 == names(Count10)[n]] <- Size
          nodes$group[id2 == names(Count10)[n]] <- names(Count10)[n]
        }else{}}
    }else{}
  }else{}

  if(!any(colnames(Graph) == "propertyLabel")){}else{
    if(any(colnames(Graph) == "parentClassLabel")){}else{
      nodes %>% rbind(nodes2) -> nodes}}
  nodes <- nodes[as.numeric(as.character(rownames(unique(nodes['id'])))),]
  
  if(is.null(Selected)){
  VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(outputNodesEdges){
    
    write.table(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), sep = ",", row.names = F) 
    write.table(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep=""), sep = ",", row.names = F) 
  }else{}
  if(output){
    VIS %>% saveNetwork(file = file)
    if(Browse){browseURL(file)}else{return(NULL)}
  } else {}
  return(VIS)
}


WikiVisNetworkFromCSV <- function(Nodes="nodes.csv", Edges="edges.csv", 
                                  Count=2, Size=10, SmallSize=5,
                                  FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                                  SEED=123, Selected=NULL, output=FALSE, 
                                  file=paste("Wikidat_visNet_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep="")){
  nodes <- read.table(file=Nodes, sep=",", header=T, stringsAsFactors =F)
  edges <- read.table(file=Edges, sep=",", header=T, stringsAsFactors =F)
  
  nodes$size <- Size
  nodes$font.size <- FontSize
  nodes$color.background <- "lightblue"
  
  if(Count > 1){
    Count10 <- table(nodes$group)
    group2 <- nodes$group
    ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        nodes$color.background[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1)
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
      }else{
        nodes$size[group2 == names(Count10)[n]] <- SmallSize
        nodes$group[group2 == names(Count10)[n]] <- "Others"
      }}
  }else{}
  
  nodes$size[group2 == "Others"] <- SmallSize
  nodes$color.background[group2 == "Others"] <- "lightblue"
  
  set.seed(SEED)
  if(is.null(Selected)){
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(output){
    VIS %>% saveNetwork(file = file)
    browseURL(file)
  } else {}
  return(VIS)
}


################################################
###  Create the Table
################################################
GraphDT <- function(Data=GraphData, output=FALSE, file=paste("DT.table_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), Rownames=FALSE, AutoWidth=TRUE, targets=c(0:(ncol(Data)-1)), width='300px'){
  DTtable <-   DT::datatable(Data,
                             rownames = Rownames,
                             caption = "Table. SPARQL results",
                             class = 'cell-border stripe',
                             filter = 'top',
                             options = list(autoWidth = AutoWidth,
                                            pageLength = 25, 
                                            lengthMenu = c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 1000, 2000),
                                            searchHighlight = TRUE,
                                            scrollX = TRUE,
                                            fixedColumns = TRUE,
                                            scrollCollapse = TRUE,
                                            columnDefs = list(list( targets=targets, width = width))
                                            ), 
                             editable = FALSE) 
  if(output){
    DTtable %>% 
    htmlwidgets::saveWidget(file = file)
    browseURL(file)
    return(DTtable)
  } else{
    return(DTtable)
  }}

GraphForm <- function(Data=GraphData, output=FALSE, file=paste("FOR.table_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep="")){
  DTtable <- formattable(Data, list(count = normalize_bar("pink")))
  if(output){
    DTtable %>% 
      as.htmlwidget() %>%
      htmlwidgets::saveWidget(file = file)
    browseURL(file)
    return(DTtable)
  } else {return(DTtable)}}
  

