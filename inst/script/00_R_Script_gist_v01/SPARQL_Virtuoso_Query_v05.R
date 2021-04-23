if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("SPARQL")){install.packages("SPARQL")}; library(SPARQL)
if(!require("readr")){install.packages("readr")}; library(readr)
if(!require("tidyverse")){install.packages("tidyverse")}; library(tidyverse)
if(!require("devtools")){install.packages("devtools")}; library(devtools)
if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("stringr")){install.packages("stringr")}; library(stringr)

################################################
## ID Search
################################################
LabelSearch <- function(Label="重合体",
                        Message=T,
                        EndPoint=EndPoint,
                        property="rdfs:subClassOf"){

LABELn <- Label
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "@ja"} else {rdfs.l <- "@en"}
#print(rdfs.l)

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

Query <- paste('
select distinct ?subject 
where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
}
', sep="")

if(Message){message(paste("Query: ", LABELn, sep=""))}else{}
A <- try(res1 <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)

if(class(A) == "try-error"){}else{}

Query2 <- paste('
select (count(distinct ?parentClass) As ?count) 
where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?subject ', property, '* ?parentClass .
}
', sep="")
try(res2 <- SPARQL(url=EndPoint, query=paste(Prefix, Query2))$results, silent = T)


Query3 <- paste('
select (count(distinct ?childClass) As ?count) 
where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?childClass ', property, '* ?subject .
}
', sep="")

try(res3 <- SPARQL(url=EndPoint, query=paste(Prefix, Query3))$results, silent = T)

return(data.frame(Label=Label, QID=as.character(res1), parentClassNum=as.numeric(res2), childClassNum=as.numeric(res3), 
       stringsAsFactors = F))

}
################################################################################################
################################################################################################





################################################################################################
################################################################################################
#EntityName="田中"; Object="wd:Q16521"; Property="wdt:P31"; lang=1
#EndPoint <- "http://kozaki-lab.osakac.ac.jp/virtuoso/sparql/"
virWD_simple_num <- function(EntityName="田中", Object="wd:Q16521", 
                             Property="wdt:P31", EndPoint=Endpoint, lang=1,
                             GraphFrom=F, Message=F){
LABEL <- EntityName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }

if(is.logical(GraphFrom)){
if(!GraphFrom){FROM <- ""}else{FROM <- ""}
}else{
if(grep("^http://", GraphFrom) == 1){
FROM <- paste("From <", GraphFrom, ">", sep="")}else{message("Syntax error")}
}

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
prefix sio: <http://semanticscience.org/resource/>
prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix iobc: <http://purl.jp/bio/4/id/>
prefix dc: <http://purl.org/dc/terms/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

Prop <- Property

Query <-paste('
SELECT (count(distinct ?subject) as ?Count) \n ',
FROM, '\n 
WHERE{
?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.','
?subject ', Prop , ' ', Object, '.',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}
suppressMessages(try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T))

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", LABEL, sep="")))}

return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################################
################################################################################################
#応急的な対応
#Data=IOBC_label01[[1781]]; virChangeLabels=T

IOBC_ConvertToJpn <- function(Data=IOBC00, virChangeLabels=T){

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
prefix sio: <http://semanticscience.org/resource/>
prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix iobc: <http://purl.jp/bio/4/id/>
prefix dc: <http://purl.org/dc/terms/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>'

EndPoint="http://kozaki-lab.osakac.ac.jp/virtuoso/sparql/"
Data$subjectLabelJpn <- NA

if(virChangeLabels){
a1 <- unique(Data$subject)
for(n1 in seq_len(length(a1))){
#n1=1
a <- grep("^http://", as.character(a1[n1])); b <- 1L
if(identical(a, b)){
a3 <- paste("<", as.character(a1[n1]), ">", sep="")
a2 <- paste("SELECT distinct ?o \n 
From <http://localhost:8890/IOBC>
WHERE { ", a3, " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
try(res <- SPARQL(url=EndPoint, query=paste(Prefix, a2)), silent = T)  
}else{
a2 <- paste("SELECT distinct ?o \n 
From <http://localhost:8890/IOBC> 
WHERE { ", a1[n1], " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
try(res <- SPARQL(url=EndPoint, query=paste(Prefix, a2)), silent = T)  
}

if(is.null(as.character(res$results)[1]) | is.na(as.character(res$results)[1])){ }else{
  if(is.integer(grep("@", as.character(res$results)[1])) && length(grep("@", as.character(res$results)[1])) == 0L){
  }else{
　res1 <- str_sub(as.character(res$results)[1], 2, -5)
  }
  Data$subjectLabelJpn[Data$subject == a1[n1]] <- res1
  }
}
return(Data)
}else{}

if(any(colnames(Data) == "childClass")){
  Data$childClassLabelJpn <- NA
  a1 <- unique(Data$subject)
  b1 <- unique(Data$childClass)
for(n1 in seq_len(length(a1))){
  #n1 =1
  a2 <- paste("SELECT distinct ?o \n 
  From <http://localhost:8890/IOBC> 
  WHERE { ", a1[n1], " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
  try(res <- SPARQL(url=EndPoint, query=paste(Prefix, a2)), silent = T)

  if(is.null(as.character(res$results)[1])){ }else{
    if(is.integer(grep("@", as.character(res$results)[1])) && length(grep("@", as.character(res$results)[1])) == 0L){
    }else{
      res1 <- str_sub(as.character(res$results)[1], 2, -5)
    }
    Data$subjectLabelJpn[Data$subject == a1[n1]] <- res1
  }
}
for(n2 in seq_len(length(b1))){
#n2 =1
b2 <- paste("SELECT distinct ?o \n 
From <http://localhost:8890/IOBC> 
WHERE { ", b1[n2], " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
try(res <- SPARQL(url=EndPoint, query=paste(Prefix, b2)), silent = T)
if(is.null(as.character(res$results)[1])){ }else{
  if(is.integer(grep("@", as.character(res$results)[1])) && length(grep("@", as.character(res$results)[1])) == 0L){
  }else{
    res2 <- str_sub(as.character(res$results)[1], 2, -5)
  }
  Data$childClassLabelJpn[Data$childClass == b1[n2]] <- res2
}}
}else{
if(any(colnames(Data) == "parentClass")){
Data$parentClassJpn <- NA
a1 <- unique(Data$subject)
b1 <- unique(Data$childClass)
for(n1 in seq_len(length(a1))){
#n1 =1
a2 <- paste("SELECT distinct ?o \n 
From <http://localhost:8890/IOBC> 
WHERE { ", a1[n1], " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
try(res <- SPARQL(url=EndPoint, query=paste(Prefix, a2)), silent = T)

if(is.null(as.character(res$results)[1])){ }else{
if(is.integer(grep("@", as.character(res$results)[1])) && length(grep("@", as.character(res$results)[1])) == 0L){
}else{
res1 <- str_sub(as.character(res$results)[1], 2, -5)
}
Data$subjectLabelJpn[Data$subject == a1[n1]] <- res1
}}
for(n2 in seq_len(length(b1))){
#n2 =1
b2 <- paste("SELECT distinct ?o \n 
From <http://localhost:8890/IOBC> 
WHERE { ", b1[n2], " rdfs:label ?o. filter(LANG(?o) = 'ja')}", sep="")
try(res <- SPARQL(url=EndPoint, query=paste(Prefix, b2)), silent = T)
if(is.null(as.character(res$results)[1])){ }else{
if(is.integer(grep("@", as.character(res$results)[1])) && length(grep("@", as.character(res$results)[1])) == 0L){
}else{
res2 <- str_sub(as.character(res$results)[1], 2, -5)
}
Data$parentClassJpn[Data$childClass == b1[n2]] <- res2
}}}}
return(Data)
}

################################################################################################
################################################################################################
#EntityName="情報学"; Message=F; PropertyView=T
#Prop1 ="skos:prefLabel"; Prop2 ="rdfs:label"; Prop3 ="skos:altLabel"; PropLabel ="skos:prefLabel"
#EndPoint="http://kozaki-lab.osakac.ac.jp/virtuoso/sparql/"; GraphFrom="http://localhost:8890/IOBC"
#Prop1opt=T; PropLabelopt=T; Prop2opt=T; Prop3opt=T

virChangeLabels <- function(EntityName="情報学", 
                            Prop1 ="skos:prefLabel", Prop1opt=T, 
                            Prop2 ="rdfs:label", Prop2opt=T, 
                            Prop3 ="skos:altLabel", Prop3opt=T, 
                            PropLabel ="skos:prefLabel", PropLabelopt=T,
                            GraphFrom="http://localhost:8890/IOBC", 
                            EndPoint="http://kozaki-lab.osakac.ac.jp/virtuoso/sparql/", 
                            PropertyView=T, Message=F){
LABEL <- EntityName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}

if(nchar(Prop1) == 0){Property1 <- ""}else{
  if(Prop1opt){
  Property1 <- paste("optional{ ?subject ", Prop1, ' \"', LABEL, '\"@', rdfs.l, ". }", sep="")  
  }else{
  Property1 <- paste("?subject ", Prop1, ' \"', LABEL, '\"@', rdfs.l, ". ", sep="")
  }
}

if(nchar(Prop2) == 0){Property2 <- ""}else{
  if(Prop2opt){
  Property2 <- paste("optional{ ?subject ", Prop2, ' \"', LABEL, '\"@', rdfs.l, ". }", sep="")  
  }else{
  Property2 <- paste("?subject ", Prop2, ' \"', LABEL, '\"@', rdfs.l, ". ", sep="")
  }
}

if(nchar(Prop3) == 0){Property3 <- ""}else{
  if(Prop3opt){
  Property3 <- paste("optional{ ?subject ", Prop3, ' \"', LABEL, '\"@', rdfs.l, ". }", sep="")  
  }else{
  Property3 <- paste("?subject ", Prop3, ' \"', LABEL, '\"@', rdfs.l, ". ", sep="")  
  }
}

if(nchar(PropLabel) == 0){return(message("You should choose the proper property."))}else{
  if(PropLabelopt){
  PropertyLabel <- paste("optional{ ?subject ", PropLabel, " ?subjectLabel.}", sep="")    
  }else{
  PropertyLabel <- paste("?subject ", PropLabel, " ?subjectLabel.", sep="")    
  }
}

if(is.logical(GraphFrom)){
if(!GraphFrom){FROM <- ""}else{FROM <- ""}
}else{
if(grep("^http://", GraphFrom) == 1){
FROM <- paste("From <", GraphFrom, ">", sep="")}else{message("Syntax error")}
}

Pref <- c("wd:", "http://www.wikidata.org/entity/",
          "wdt:", "http://www.wikidata.org/prop/direct/",
          "rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
          "rdfs:", "http://www.w3.org/2000/01/rdf-schema#",
          "skos:", "http://www.w3.org/2004/02/skos/core#",
          "sio:", "http://semanticscience.org/resource/",
          "xkos:", "http://rdf-vocabulary.ddialliance.org/xkos#",
          "owl:", "http://www.w3.org/2002/07/owl#",
          "iobc:", "http://purl.jp/bio/4/id/",
          "dc:", "http://purl.org/dc/terms/",
          "dct:", "http://purl.org/dc/terms/",
          "foaf:", "http://xmlns.com/foaf/0.1/",
          "", "<", "", ">")

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
prefix sio: <http://semanticscience.org/resource/>
prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix iobc: <http://purl.jp/bio/4/id/>
prefix dc: <http://purl.org/dc/terms/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>'

Query <-paste('
SELECT distinct ?subject  ?subjectLabel \n ',
FROM,' 
WHERE {',
Property1,
Property2,
Property3,
PropertyLabel,'
}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}
A <- try(res <- SPARQL(url=EndPoint, query=paste(Prefix, Query)), silent = T)
if(class(A) == "try-error"){}else{SPA <- res$results}

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

for(n in seq_len(length(Pref)/2)){
#n <- 9
z <- grep(Pref[2*n], SPA$subject)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA$subject <- gsub(Pref[2*n], Pref[2*n-1], SPA$subject), silent = T)
}}

if(is.integer(grep("@", SPA$subjectLabel)) && length(grep("@", SPA$subjectLabel)) == 0L){
}else{
 zz <- grep("@", SPA$subjectLabel)
 SPA$subjectLabel[zz] <- str_sub(SPA$subjectLabel[zz], 2, -5)
}

if(PropertyView){
  SPA$subjectProperty <- PropLabel
}else{}

return(data.frame(SPA, stringsAsFactors = F))
}

########################################################################
########################################################################
#EntityName="情報学"; Upper=F; Depth=5; notIN=NULL; output=FALSE; FileName="RDFGraph.csv";HalfWay=FALSE
#EndPoint="http://kozaki-lab.osakac.ac.jp/virtuoso/sparql/"; GraphFrom="http://localhost:8890/IOBC"
#PropertyView=TRUE; Prop0="skos:prefLabel"; PropOpt="skos:prefLabel"; Prop1="rdfs:subClassOf"; Prop2="rdfs:subClassOf"
#EntityName=LabelDat01[2]; Message=F
#IDorLabel="ID"

#EntityName="iobc:200906072361242448"
#EntityName="http://purl.jp/bio/4/subject/LS_ur_c/OWLClass_00000000000000000451"

#EntityName="iobc:201006004568224534"
#EntityName=IOBC_label03[287]; Upper=T

#IDで検索するか、ラベルで検索するかの切り替えができる
virGraph_v1 <- function(EntityName="情報学", Upper=F, Depth=5, IDorLabel="ID",
                        notIN=NULL, output=FALSE, FileName="RDFGraph.csv", 
                        PropertyView=TRUE,
                        Prop0="rdfs:label", PropOpt="skos:prefLabel", 
                        Prop1="rdfs:subClassOf", Prop2="rdfs:subClassOf",
                        GraphFrom=FALSE, HalfWay=FALSE, EndPoint=Endpoint, 
                        Message=F, ...){
LABEL <- EntityName
SPA2 <- NULL
if(HalfWay){SPAresults <<- NULL}
x <- 0
y <- 0

if(Upper){
SearchQuery01 <- "?parentClass ?parentClassj ?parentClasse"
SearchQuery02 <- paste("?subject ", Prop1, " ?parentClass.", sep="")
SearchQuery03 <- paste('
optional { ?subject ', PropOpt ,' ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ?subject ', PropOpt ,' ?subjecte . filter(LANG(?subjecte) = "en") }
optional { ?parentClass ', PropOpt ,' ?parentClassj . filter(LANG(?parentClassj) = "ja") }
optional { ?parentClass ', PropOpt ,' ?parentClasse . filter(LANG(?parentClasse) = "en") }', sep="")
} else {
SearchQuery01 <- "?childClass ?childClassj ?childClasse"
SearchQuery02 <- paste("?childClass ", Prop1, " ?subject.", sep="")
SearchQuery03 <- paste('
optional { ?subject ', PropOpt ,' ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ?subject ', PropOpt ,' ?subjecte . filter(LANG(?subjecte) = "en") }
optional { ?childClass ', PropOpt ,' ?childClassj . filter(LANG(?childClassj) = "ja") }
optional { ?childClass ', PropOpt ,' ?childClasse . filter(LANG(?childClasse) = "en") }', sep="")
}

if(PropertyView){
  SearchProperty01 <- Prop1
  SearchProperty02 <- Prop2
}else{}

if(is.null(notIN)){ 
notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

if(is.logical(GraphFrom)){
if(!GraphFrom){FROM <- ""}else{FROM <- ""}
}else{
if(grep("^http://", GraphFrom) == 1){
FROM <- paste("From <", GraphFrom, ">", sep="")}else{message("Syntax error")}
}

Pref <- c("wd:", "http://www.wikidata.org/entity/",
          "wdt:", "http://www.wikidata.org/prop/direct/",
          "rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
          "rdfs:", "http://www.w3.org/2000/01/rdf-schema#",
          "skos:", "http://www.w3.org/2004/02/skos/core#",
          "sio:", "http://semanticscience.org/resource/",
          "xkos:", "http://rdf-vocabulary.ddialliance.org/xkos#",
          "owl:", "http://www.w3.org/2002/07/owl#",
          "iobc:", "http://purl.jp/bio/4/id/",
          "iobc2:", "http://purl.jp/bio/4/subject/LS_ur_c/",
          "iobc3:", "http://purl.jp/bio/4/subject/",
          "dc:", "http://purl.org/dc/terms/",
          "dct:", "http://purl.org/dc/terms/",
          "foaf:", "http://xmlns.com/foaf/0.1/",
          "", "<", "", ">")

SPA1 <- NULL
x <- x + 1
Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
prefix sio: <http://semanticscience.org/resource/>
prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix iobc: <http://purl.jp/bio/4/id/>
prefix iobc2: <http://purl.jp/bio/4/subject/LS_ur_c/>
prefix iobc3: <http://purl.jp/bio/4/subject/>
prefix dc: <http://purl.org/dc/terms/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

for(n in seq_len(length(Pref)/2)){
#n <- 9
z <- grep(Pref[2*n], LABEL)
if(is.integer(z) && length(z) == 0L){}else{
try(LABEL <- gsub(Pref[2*n], Pref[2*n-1], LABEL), silent = T)
}}

a0 <- grep("^http://", as.character(LABEL)); b0 <- 1L
if(identical(a0, b0)){LABEL <- paste("<", LABEL, ">", sep="")}else{}

if(as.character(IDorLabel) == "1" | as.character(IDorLabel) == "ID"){
#ID
SearchQueryIDorLabel <- ""
if(Upper){
SearchQuery02 <- paste(LABEL, " ", Prop1, " ?parentClass.", sep="")
SearchQuery03 <- paste('
optional { ', LABEL , ' ' , PropOpt ,' ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ', LABEL , ' ' , PropOpt ,' ?subjecte . filter(LANG(?subjecte) = "en") }
optional { ?parentClass ', PropOpt ,' ?parentClassj . filter(LANG(?parentClassj) = "ja") }
optional { ?parentClass ', PropOpt ,' ?parentClasse . filter(LANG(?parentClasse) = "en") }', sep="")
} else {
SearchQuery02 <- paste("?childClass ", Prop1, " ", LABEL," .", sep="")
SearchQuery03 <- paste('
optional { ', LABEL , ' ' , PropOpt ,' ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ', LABEL , ' ' , PropOpt ,' ?subjecte . filter(LANG(?subjecte) = "en") }
optional { ?childClass ', PropOpt ,' ?childClassj . filter(LANG(?childClassj) = "ja") }
optional { ?childClass ', PropOpt ,' ?childClasse . filter(LANG(?childClasse) = "en") }', sep="")
}
}else{
#Label
  SearchQueryIDorLabel <- paste('{?subject ', Prop0 ,' \"', LABELn, '\"@ja .} UNION {?subject ', Prop0, ' \"', LABELn, '\"@en .}', sep="")
}

if(T){
LABELn <- LABEL

Query <-paste('
SELECT distinct ?subject ?subjectj ?subjecte ', SearchQuery01, ' \n ',
FROM, '\n 
WHERE{',
SearchQueryIDorLabel,
SearchQuery02,
SearchQuery03,
notIN01,
'}', sep="")
}

if(F){
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
Query <-paste('
SELECT distinct ?subject ?subjectj ?subjecte ', SearchQuery01, ' \n ',
FROM, '\n 
WHERE{
?subject ', Prop0 ,' \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,
SearchQuery03,
notIN01,
'}', sep="")
}

y <- y + 1
if(Message){message(paste("Query: ", LABELn, sep=""))}else{}
A <- try(res <- SPARQL(url=EndPoint, query=paste(Prefix, Query)), silent = T)
if(class(A) == "try-error"){}else{SPA <- res$results}

if(is.null(SPA)){ }else{
  if(nrow(SPA) == 0){ }else{
if(as.character(IDorLabel) == "1" | as.character(IDorLabel) == "ID"){
  SPA$subject <- LABEL 
}else{}
}}

#head(SPA)
if(nrow(SPA) == 0){ }else{
if(any(is.na(SPA))){
if(Upper){
SPA$subjectj[is.na(SPA$subjectj)] <- SPA$subjecte[is.na(SPA$subjectj)]
SPA$parentClassj[is.na(SPA$parentClassj)] <- SPA$parentClasse[is.na(SPA$parentClassj)]
}else{
SPA$subjectj[is.na(SPA$subjectj)] <- SPA$subjecte[is.na(SPA$subjectj)]
SPA$childClassj[is.na(SPA$childClassj)] <- SPA$childClasse[is.na(SPA$childClassj)]
}}else{}}

for(n in seq_len(length(Pref)/2)){
#n <- 9
z <- grep(Pref[2*n], SPA$subject)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA$subject <- gsub(Pref[2*n], Pref[2*n-1], SPA$subject), silent = T)
}}

for(n in seq_len(length(Pref)/2)){
#n <- 1
if(Upper){
z <- grep(Pref[2*n], SPA$parentClass)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA$parentClass <- gsub(">", "", SPA$parentClass), silent = T)
try(SPA$parentClass <- gsub(Pref[2*n], Pref[2*n-1], SPA$parentClass), silent = T)
}}else{
z <- grep(Pref[2*n], SPA$childClass)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA$childClass <- gsub(">", "", SPA$childClass), silent = T)
try(SPA$childClass <- gsub(Pref[2*n], Pref[2*n-1], SPA$childClass), silent = T)
}}}

#head(SPA)
if(is.null(SPA)){ }else{
if(is.integer(grep("@", SPA$subjectj)) && length(grep("@", SPA$subjectj)) == 0L){
}else{
 zz <- grep("@", SPA$subjectj)
 SPA$subjectj[zz] <- str_sub(SPA$subjectj[zz], 2, -5)
}
if(is.integer(grep("@", SPA$subjecte)) && length(grep("@", SPA$subjecte)) == 0L){
}else{
 zz <- grep("@", SPA$subjecte)
 SPA$subjecte[zz] <- str_sub(SPA$subjecte[zz], 2, -5)
}
  
if(Upper){
if(is.integer(grep("@", SPA$parentClassj)) && length(grep("@", SPA$parentClassj)) == 0L){
}else{
 zz <- grep("@", SPA$parentClassj)
 SPA$parentClassj[zz] <- str_sub(SPA$parentClassj[zz], 2, -5)
}
if(is.integer(grep("@", SPA$parentClasse)) && length(grep("@", SPA$parentClasse)) == 0L){
}else{
 zz <- grep("@", SPA$parentClasse)
 SPA$parentClasse[zz] <- str_sub(SPA$parentClasse[zz], 2, -5)
}}else{
if(is.integer(grep("@", SPA$childClassj)) && length(grep("@", SPA$childClassj)) == 0L){
}else{
 zz <- grep("@", SPA$childClassj)
 SPA$childClassj[zz] <- str_sub(SPA$childClassj[zz], 2, -5)
}
if(is.integer(grep("@", SPA$childClasse)) && length(grep("@", SPA$childClasse)) == 0L){
}else{
 zz <- grep("@", SPA$childClasse)
 SPA$childClasse[zz] <- str_sub(SPA$childClasse[zz], 2, -5)
}}}

if(is.null(SPA) | nrow(SPA) == 0){ }else{
if(PropertyView){
SPA1 %>% rbind(data.frame(SPA, group=LABELn, Property=SearchProperty01, Depth=x, stringsAsFactors = F)) -> SPA1
}else{
SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1
}}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}else{ }

if(is.null(SPA1) | Depth == x){
if(is.null(SPA1) && x == 1){
if(Message){
  cat(paste("Finished. \n Number of SPARQL Query: ", y, "\n Depth from the Query: ", x, "\n", sep=""))
}else{}
return(SPA1); break
}
if(Message){
  cat(paste("Finished. \n Number of SPARQL Query: ", y, "\n Depth from the Query : ", x, "\n", sep=""))
}else{}

if(Upper){
SPA2$Unique <- paste(SPA2$subject, ".", SPA2$Property, ".", SPA2$parentClass, ".", SPA2$group, sep="")
}else{
SPA2$Unique <- paste(SPA2$subject, ".", SPA2$Property, ".", SPA2$childClass, ".", SPA2$group, sep="")
}
SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),]

rownames(SPA3) <- 1:nrow(SPA3)
if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}
return(SPA3); break } else { }

#head(SPA1)
if(Upper){
  LABEL01 <- SPA1[,4:6]
  LABEL01a <- LABEL01[as.numeric(rownames(unique(LABEL01["parentClass"]))),]
  LABELg  <- LABEL01a[,1]
  LABEL02 <- LABEL01a[,2]
  LABEL03 <- LABEL01a[,3]
}else{
  LABEL01 <- SPA1[,4:6]
  LABEL01a <- LABEL01[as.numeric(rownames(unique(LABEL01["childClass"]))),]
  LABELg  <- LABEL01a[,1]
  LABEL02 <- LABEL01a[,2]
  LABEL03 <- LABEL01a[,3]
}

if(is.integer(grep("@", LABEL02)) && length(grep("@", LABEL02)) == 0L){
}else{
 zz <- grep("@", LABEL02)
 LABEL02[zz] <- str_sub(LABEL02[zz], 2, -5)
}
if(is.integer(grep("@", LABEL03)) && length(grep("@", LABEL03)) == 0L){
}else{
 zz <- grep("@", LABEL03)
 LABEL03[zz] <- str_sub(LABEL03[zz], 2, -5)
}

repeat {
SPA1 <- NULL
x <- x + 1
LABEL <- LABELg
LABELL <- LABEL02
LABELLL <- LABEL03

for(n in seq_len(length(LABEL))){
# n <- 1
LABELn <- LABEL[n]
LABELnL <- LABELL[n]
LABELnLL <- LABELLL[n]

for(n in seq_len(length(Pref)/2)){
#n <- 9
z <- grep(Pref[2*n], LABELn)
if(is.integer(z) && length(z) == 0L){}else{
try(LABELn <- gsub(Pref[2*n], Pref[2*n-1], LABELn), silent = T)
}}

a0 <- grep("^http://", as.character(LABELn)); b0 <- 1L
if(identical(a0, b0)){LABELn <- paste("<", LABELn, ">", sep="")}else{}

if(Upper){
SearchQuery04 <- paste(LABELn, " ", Prop2, " ?parentClass.", sep="")
SearchQuery05 <- paste('
optional { ?parentClass ', PropOpt ,' ?parentClassj . filter(LANG(?parentClassj) = "ja") }
optional { ?parentClass ', PropOpt ,' ?parentClasse . filter(LANG(?parentClasse) = "en") }', sep="")
} else {
SearchQuery04 <- paste("?childClass ", Prop2, " ", LABELn, ".", sep="")
SearchQuery05 <- paste('
optional { ?childClass ', PropOpt ,' ?childClassj . filter(LANG(?childClassj) = "ja") }
optional { ?childClass ', PropOpt ,' ?childClasse . filter(LANG(?childClasse) = "en") }', sep="")
}

Query <-paste('
SELECT distinct ?subject ?subjectj ?subjecte ', SearchQuery01, ' \n ',
FROM, '\n 
WHERE{',
SearchQuery04,
SearchQuery05,
notIN01,
'}', sep="")

y <- y + 1
if(Message){message(paste("Query: ", LABELnL, sep=""))}else{}
A <- try(res <- SPARQL(url=EndPoint, query=paste(Prefix, Query)), silent = T)
if(class(A) == "try-error"){}else{SPA <- res$results}

if(nrow(SPA) == 0){ }else{
if(any(is.na(SPA))){
if(Upper){
SPA$parentClassj[is.na(SPA$parentClassj)] <- SPA$parentClasse[is.na(SPA$parentClassj)]
}else{
SPA$childClassj[is.na(SPA$childClassj)] <- SPA$childClasse[is.na(SPA$childClassj)]
}}else{}
SPA$subject <- LABELn; SPA$subjectj <- LABELnL; SPA$subjecte <- LABELnLL
if(PropertyView){
SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Property=SearchProperty02, Depth=x, stringsAsFactors = F)) -> SPA1
}else{
SPA1 %>% rbind(data.frame(SPA, group=LABELnL, Depth=x, stringsAsFactors = F)) -> SPA1
}}
}

#head(SPA1)
for(n in seq_len(length(Pref)/2)){
#n <- 1
if(Upper){
z <- grep(Pref[2*n], SPA1$parentClass)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA1$parentClass <- gsub(">", "", SPA1$parentClass), silent = T)
try(SPA1$parentClass <- gsub(Pref[2*n], Pref[2*n-1], SPA1$parentClass), silent = T)
}}else{
z <- grep(Pref[2*n], SPA1$childClass)
if(is.integer(z) && length(z) == 0L){}else{
try(SPA1$childClass <- gsub(">", "", SPA1$childClass), silent = T)
try(SPA1$childClass <- gsub(Pref[2*n], Pref[2*n-1], SPA1$childClass), silent = T)
}}}

#head(SPA1)
if(is.null(SPA1)){ }else{
if(is.integer(grep("@", SPA1$subjectj)) && length(grep("@", SPA1$subjectj)) == 0L){
}else{
 zz <- grep("@", SPA1$subjectj)
 SPA1$subjectj[zz] <- str_sub(SPA1$subjectj[zz], 2, -5)
}
if(is.integer(grep("@", SPA1$subjecte)) && length(grep("@", SPA1$subjecte)) == 0L){
}else{
 zz <- grep("@", SPA1$subjecte)
 SPA1$subjecte[zz] <- str_sub(SPA1$subjecte[zz], 2, -5)
}
  
if(Upper){
if(is.integer(grep("@", SPA1$parentClassj)) && length(grep("@", SPA1$parentClassj)) == 0L){
}else{
 zz <- grep("@", SPA1$parentClassj)
 SPA1$parentClassj[zz] <- str_sub(SPA1$parentClassj[zz], 2, -5)
}
if(is.integer(grep("@", SPA1$parentClasse)) && length(grep("@", SPA1$parentClasse)) == 0L){
}else{
 zz <- grep("@", SPA1$parentClasse)
 SPA1$parentClasse[zz] <- str_sub(SPA1$parentClasse[zz], 2, -5)
}}else{
if(is.integer(grep("@", SPA1$childClassj)) && length(grep("@", SPA1$childClassj)) == 0L){
}else{
 zz <- grep("@", SPA1$childClassj)
 SPA1$childClassj[zz] <- str_sub(SPA1$childClassj[zz], 2, -5)
}
if(is.integer(grep("@", SPA1$childClasse)) && length(grep("@", SPA1$childClasse)) == 0L){
}else{
 zz <- grep("@", SPA1$childClasse)
 SPA1$childClasse[zz] <- str_sub(SPA1$childClasse[zz], 2, -5)
}}}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
#head(SPA2)

if(is.null(SPA1) | Depth == x) {
if(Message){
  cat(paste("Finished. \n Number of SPARQL Query: ", y, "\n Depth from the Query: ", x, "\n", sep=""))
}else{}

if(Upper){
SPA2$Unique <- paste(SPA2$subject, ".", SPA2$Property, ".", SPA2$parentClass, ".", SPA2$group, sep="")
}else{
SPA2$Unique <- paste(SPA2$subject, ".", SPA2$Property, ".", SPA2$childClass, ".", SPA2$group, sep="")
}
SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),]

rownames(SPA3) <- 1:nrow(SPA3)
if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}
return(SPA3); break } else {}

#head(SPA1)
if(Upper){
  LABEL01 <- SPA1[,4:6]
  LABEL01a <- LABEL01[as.numeric(rownames(unique(LABEL01["parentClass"]))),]
  LABELg  <- LABEL01a[,1]
  LABEL02 <- LABEL01a[,2]
  LABEL03 <- LABEL01a[,3]
}else{
  LABEL01 <- SPA1[,4:6]
  LABEL01a <- LABEL01[as.numeric(rownames(unique(LABEL01["childClass"]))),]
  LABELg  <- LABEL01a[,1]
  LABEL02 <- LABEL01a[,2]
  LABEL03 <- LABEL01a[,3]
}

if(is.integer(grep("@", LABEL02)) && length(grep("@", LABEL02)) == 0L){
}else{
 zz <- grep("@", LABEL02)
 LABEL02[zz] <- str_sub(LABEL02[zz], 2, -5)
}
if(is.integer(grep("@", LABEL03)) && length(grep("@", LABEL03)) == 0L){
}else{
 zz <- grep("@", LABEL03)
 LABEL03[zz] <- str_sub(LABEL03[zz], 2, -5)
}
}
}

################################################
#integBioGraph
#EndPoint="https://integbio.jp/rdf/sparql"

#OntoBeeGraph
#EndPoint="http://sparql.hegroup.org/sparql/"
################################################

##Graph
#GraphData <- Results
#Graph = GraphData; output=FALSE; file="Wikidat_Network3d01.html"; linkDistance= 40; charge=-30; fontSize=6

CreateNetwork3d_v1 <- function(Graph = GraphData, output=TRUE, 
                            file=paste("CreateNetwork_", format(Sys.time(), "%y%m%d%H%M"),".html", sep=""),
                            linkDistance= 40, charge=-30, fontSize=6,
                            Source=2, Target=6){
if(is.null(Graph)){ message(paste("No Graph Data", sep="")) } else {
Links <- data.frame(source= Graph[,Source], target=Graph[,Target], value=NA, stringsAsFactors = F)
N3d <- simpleNetwork(Links,
 linkDistance = linkDistance, charge = charge, fontSize = fontSize, fontFamily = "Arial",
 linkColour = "#666", nodeColour = "#225c87", opacity = 0.7, zoom = T) 
if(output){
N3d %>% htmlwidgets::saveWidget(file = file)
browseURL(file)
} else {}
return(N3d)
}}

CreateNetwork3d_v2 <- function(Graph = GraphData, output=TRUE, 
                            file=paste("CreateNetwork_", format(Sys.time(), "%y%m%d%H%M"),".html", sep=""),
                            linkDistance= 40, charge=-30, fontSize=6,
                            Source=2, Target=6){
if(is.null(Graph)){ message(paste("No Graph Data", sep="")) } else {
Links <- data.frame(source= Graph[,Source], target=Graph[,Target], value=NA, stringsAsFactors = F)
N3d <- simpleNetwork(Links,
 linkDistance = linkDistance, charge = charge, fontSize = fontSize, fontFamily = "Arial",
 linkColour = "#666", nodeColour = "#225c87", opacity = 0.7, zoom = T) 
if(output){
N3d %>% htmlwidgets::saveWidget(file = file)
browseURL(file)
} else {}
return(N3d)
}}

################################################
## サブクラス階層の取得
################################################
#EntityName="polymer"; Upper=F; Depth=5;notIN=NULL; output=FALSE; FileName="RDFGraph.csv";HalfWay=FALSE; EndPoint=Endpoint

virGraph <- function(EntityName="データサイエンス", Upper=TRUE, Depth=5, 
 notIN=NULL, output=FALSE, FileName="RDFGraph.csv", HalfWay=FALSE, 
 EndPoint=Endpoint, Message=F, ...){
LABEL <- EntityName
SPA2 <- NULL
if(HalfWay){SPAresults <<- NULL}
x <- 0
y <- 0
if(Upper){ 
SearchQuery01 <- "?parentClass ?parentClassj ?parentClasse"
SearchQuery02 <- "?subject wdt:P279 ?parentClass."
SearchQuery03 <- ' 
optional { ?subject rdfs:label ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ?subject rdfs:label ?subjecte . filter(LANG(?subjecte) = "en") }
optional {?parentClass rdfs:label ?parentClassj . filter(LANG(?parentClassj) = "ja")}
optional { ?parentClass rdfs:label ?parentClasse . filter(LANG(?parentClasse) = "en") }'
} else {
SearchQuery01 <- "?childClass ?childClassj ?childClasse"
SearchQuery02 <- "?childClass wdt:P279 ?subject."
SearchQuery03 <- ' 
optional { ?subject rdfs:label ?subjectj . filter(LANG(?subjectj) = "ja") }
optional { ?subject rdfs:label ?subjecte . filter(LANG(?subjecte) = "en") }
optional { ?childClass rdfs:label ?childClassj . filter(LANG(?childClassj) = "ja") }
optional { ?childClass rdfs:label ?childClasse . filter(LANG(?childClasse) = "en") }' 
}

if(is.null(notIN)){ 
notIN01 <- "" 
} else {notIN01 <- paste("FILTER ( ?subject NOT IN (", notIN, " ) )", sep="")}

repeat {
SPA1 <- NULL
x <- x + 1
Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
prefix sio: <http://semanticscience.org/resource/>
prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix iobc: <http://purl.jp/bio/4/id/>
prefix dc: <http://purl.org/dc/terms/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

for(n in seq_len(length(LABEL))){
# n <- 10
LABELn <- LABEL[n]
if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}

Query <-paste('
SELECT distinct ?subject ?subjectj ?subjecte ', SearchQuery01, '
WHERE 
{
?subject rdfs:label \"', LABELn, '\"@', rdfs.l, '.',
SearchQuery02,
SearchQuery03,
notIN01,
'}', sep="")

y <- y + 1
if(Message){message(paste("Query: ", LABELn, sep=""))}else{}
A <- try(res <- SPARQL(url=EndPoint, query=paste(Prefix, Query)), silent = T)
if(class(A) == "try-error"){}else{SPA <- res$results}

if(nrow(SPA) == 0){ }else{
if(any(is.na(SPA))){
if(Upper){
SPA$subjectj[is.na(SPA$subjectj)] <- SPA$subjecte[is.na(SPA$subjectj)]
SPA$parentClassj[is.na(SPA$parentClassj)] <- SPA$parentClasse[is.na(SPA$parentClassj)]
}else{
SPA$subjectj[is.na(SPA$subjectj)] <- SPA$subjecte[is.na(SPA$subjectj)]
SPA$childClassj[is.na(SPA$childClassj)] <- SPA$childClasse[is.na(SPA$childClassj)]
}}else{}
SPA1 %>% rbind(data.frame(SPA, group=LABELn, Depth=x, stringsAsFactors = F)) -> SPA1
}}

if(is.null(SPA1)){ }else{SPA2 %>% rbind(SPA1) -> SPA2}
if(HalfWay){SPA2 ->> SPAresults}
if(is.null(SPA1) | Depth == x) {
cat(paste("Finished. \n Number of SPARQL Query: ", y, "\n Depth from the Query : ", x, "\n", sep=""))
if(Upper){
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$parentClass, ".", SPA2$group, sep="")
}else{
SPA2$Unique <- paste(SPA2$subject, ".",SPA2$childClass, ".", SPA2$group, sep="")
}
SPA3 <- SPA2[as.numeric(as.character(rownames(unique(SPA2['Unique'])))),]

rownames(SPA3) <- 1:nrow(SPA3)
if(output){ write.table(SPA3, file = FileName, sep = ",", row.names = F) }else{}
return(SPA3); break } else {}
if(Upper){
LABEL <- unique(SPA1$parentClassj)
LABEL <- str_sub(LABEL, 2, -5)
} else { 
LABEL <- unique(SPA1$childClassj) 
LABEL <- str_sub(LABEL, 2, -5)
}}}
