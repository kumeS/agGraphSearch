
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
###  Search wikidata ID
## only match ID
################################################
wikiWD <- function(EntityName="データサイエンス", lang=1){
LABEL <- EntityName
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
#EntityName="田中"; Property="wdt:P31"; Object="wd:Q101352"; lang=1
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

###########################################################################
#WDname="wd:Q12159869"; Property="wdt:P31"; Object="?o"; lang=1; GroupBy=FALSE;  DISTINCT=T; PREFIX=T

wikiWD_QID_PropertyP31_Object_v01 <- function(WDname="wd:Q12159869", Property="wdt:P31", 
                                           Object="?o", lang=1, DISTINCT=T, PREFIX=T){
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
if(DISTINCT){
  Distinct00 <- paste("distinct ", Object, " ", Object, "Label ", sep="")
}else{
  Distinct00 <- paste(" ", Object, " ", Object, "Label ", sep="")
}

Query <-paste('
SELECT ', Distinct00, '
WHERE { ', 
QID, ' ', Prop , ' ', Object, '. 
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'} ', sep="")

#message(paste("Query: ", QID, sep=""))
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(PREFIX){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}else{}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
try(SPA$QID <- QID, silent = T)
return(data.frame(SPA, stringsAsFactors = F))
}


##########################################################################
##########################################################################

wikiWD_R <- function(EntityName="データサイエンス"){
LABEL <- EntityName
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
#EntityName="データサイエンス"; lang=1; Message=T; FilterRegex=F; DirSAVE=F; Dir="R01_Results"
#EntityName="重合体"; lang=1; Message=T; FilterRegex=F; DirSAVE=F; Dir="R01_Results"
#EntityName="ポリ尿素"; lang=1; Message=T; FilterRegex=F; DirSAVE=F; Dir="R01_Results"

wikiCount_Label_Num <- function(EntityName="データサイエンス", 
                                lang=1, Message=T, FilterRegex=F, DirSAVE=T, Dir="R01_Results"){

if(DirSAVE){if(!dir.exists(Dir)){dir.create(Dir)}}else{}

LABEL <- EntityName
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

if(FilterRegex){
LAB00 <- paste('?subject rdfs:label ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")  
LAB01 <- paste('?subject skos:altLabel ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")  
}else{
LAB00 <- paste('?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '. ', sep="")
LAB01 <- paste('?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '. ', sep="")
}

Query01 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_Label)
WHERE {',LAB00,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}', sep="")
if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

suppressMessages(try(SPA01 <- query_wikidata(paste(Prefix, Query01), format = "simple"), silent = T))
if(exists("SPA01")){}else{
  return(message("Perhaps No Internet Services"))}

Query02 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_AltLabel)
WHERE {',LAB01,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA02 <- query_wikidata(paste(Prefix, Query02), format = "simple"), silent = T))

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)
WHERE {',LAB00,'
?subject wdt:P279 ?parentClass.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA03A <- query_wikidata(paste(Prefix, Query03A), format = "simple"), silent = T))

Query03B <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_altLabel)
WHERE {',LAB01,'
?subject wdt:P279 ?parentClass.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA03B <- query_wikidata(paste(Prefix, Query03B), format = "simple"), silent = T))

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)
WHERE {',LAB00,'
?childClass wdt:P279 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA04A <- query_wikidata(paste(Prefix, Query04A), format = "simple"), silent = T))

Query04B <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_altLabel)
WHERE {',LAB01,'
?childClass wdt:P279 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA04B <- query_wikidata(paste(Prefix, Query04B), format = "simple"), silent = T))

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)
WHERE {',LAB00,'
?instance wdt:P31 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA05A <- query_wikidata(paste(Prefix, Query05A), format = "simple"), silent = T))

Query05B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_altLabel)
WHERE {',LAB01,'
?instance wdt:P31 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA05B <- query_wikidata(paste(Prefix, Query05B), format = "simple"), silent = T))

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)
WHERE {',LAB00,'
?subject wdt:P31 ?instance.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA06A <- query_wikidata(paste(Prefix, Query06A), format = "simple"), silent = T))

Query06B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_altLabel)
WHERE {',LAB01,'
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
                  Hit_subClassOf_Instance=SPA03A.SPA03B.SPA04A.SPA04B+SPA05A.SPA05B.SPA06A.SPA06B,
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

if(DirSAVE){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
saveRDS(SPA, file = paste(Dir, "/", LABEL00, ".Rdata", sep=""), compress = TRUE)
}else{}

return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################
################################################################################
#ラベルヒット、別名ヒット、インスタンスヒット、クラス関係ヒットとかも追加
#EntityName="データサイエンス"; lang=1; Message=T;  Dir="R01_Results"; SAVE=T

wikiCount_AmbiguousLabel_Num <- function(EntityName="データサイエンス", lang=1, Message=T, Dir="R01_Results", DirSAVE=F){

if(DirSAVE){if(!dir.exists(Dir)){dir.create(Dir)}}

LABEL <- EntityName
#if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }
#SPA01 <- NA; SPA02 <- NA; SPA03A <- NA; SPA03B <- NA; SPA04A <- NA; SPA04B <- NA; SPA05A <- NA; SPA05B <- NA; SPA06A <- NA; SPA05B <- NA

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

SER00 <- paste('
SERVICE wikibase:mwapi {
bd:serviceParam wikibase:api "EntitySearch".
bd:serviceParam wikibase:endpoint "www.wikidata.org".
bd:serviceParam mwapi:search \"', LABEL, '\".
bd:serviceParam mwapi:language "ja".
?subject wikibase:apiOutputItem mwapi:item.
?num wikibase:apiOrdinal true.
} ', sep="")

Query01 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_Label)
WHERE {',SER00,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}ORDER BY ?num LIMIT 1 ', sep="")
if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

suppressMessages(try(SPA01 <- query_wikidata(paste(Prefix, Query01), format = "simple"), silent = T))
if(exists("SPA01")){}else{
return(message("Perhaps No Internet Services"))}

Query02 <-paste('
SELECT ?subject 
WHERE {',SER00,'
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }',
'}ORDER BY ?num LIMIT 1 ', sep="")
suppressMessages(try(SPA02 <- query_wikidata(paste(Prefix, Query02), format = "simple"), silent = T))
try(SPA02.g <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(SPA02)[1]), silent = T)

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)
WHERE {',SER00,'
?subject wdt:P279 ?parentClass.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA03A <- query_wikidata(paste(Prefix, Query03A), format = "simple"), silent = T))

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)
WHERE {',SER00,'
?childClass wdt:P279 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA04A <- query_wikidata(paste(Prefix, Query04A), format = "simple"), silent = T))

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)
WHERE {',SER00,'
?instance wdt:P31 ?subject.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA05A <- query_wikidata(paste(Prefix, Query05A), format = "simple"), silent = T))

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)
WHERE {',SER00,'
?subject wdt:P31 ?instance.
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". }
}', sep="")
suppressMessages(try(SPA06A <- query_wikidata(paste(Prefix, Query06A), format = "simple"), silent = T))

SPA <- data.frame(LABEL=LABEL,
                  QID=SPA02.g, 
                  Hit_Label=as.numeric(SPA01), 
                  Hit_ParentClass_InstanceOf=as.numeric(SPA03A)  + as.numeric(SPA06A),
                  Hit_subClassOf=as.numeric(SPA03A) + as.numeric(SPA04A),
                  Hit_Instance=as.numeric(SPA05A) + as.numeric(SPA06A),
                  Hit_ParentClass=as.numeric(SPA03A),
                  Hit_ChildClass=as.numeric(SPA04A),
                  Hit_InstanceOf=as.numeric(SPA06A),
                  Hit_Has_Instance=as.numeric(SPA05A),
                  stringsAsFactors=F)

if(DirSAVE){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
try(saveRDS(SPA, file = paste(Dir, "/", LABEL00, ".Rdata", sep=""), compress = TRUE), silent=T)
}else{}

return(data.frame(SPA, stringsAsFactors = F))
}


################################################################################
################################################################################
################################################################################
#QID_Name="wd:Q2374463"; lang=1; Message=T
################################################################################
#QID_Name="wd:Q2374463"; lang=1; Message=F

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


#クラス名と一致したクラスの別名検索
wikiWD_Alt <- function(EntityName="データサイエンス", lang=1){
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

##EntityNameへのインスタンス関係
wikiWD_Ins <- function(EntityName="データサイエンス", lang=1){
LABEL <- EntityName
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

#EntityName <- "CU化学式"; lang=1
isNumeric <- function(st) {
  return(!is.na(suppressWarnings(as.numeric(st))))
}


wikiWD2_Alt <- function(EntityName="データサイエンス", lang=1){
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
#EntityName="重合体"; AltLabel=F; lang=1; Message=F

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


################################################
##wikiWD_R + wikiWD2_Alt
################################################
wikiWD3 <- function(EntityName="データサイエンス"){
LABEL <- EntityName
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
wikiWD4 <- function(EntityName="データサイエンス"){
LABEL <- EntityName
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