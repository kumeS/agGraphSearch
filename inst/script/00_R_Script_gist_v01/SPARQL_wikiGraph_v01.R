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
  if(!any(ls() == "SPA3")){ return(NULL); suppressMessages(try(stop(), silent = T)) }else{  
    if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}}
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
    

