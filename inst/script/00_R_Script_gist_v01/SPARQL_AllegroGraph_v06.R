if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("SPARQL")){install.packages("SPARQL")}; library(SPARQL)
if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)

if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("readr")){install.packages("readr")}; library(readr)
if(!require("tidyverse")){install.packages("tidyverse")}; library(tidyverse)
if(!require("devtools")){install.packages("devtools")}; library(devtools)
if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("stringr")){install.packages("stringr")}; library(stringr)
if(!require("furrr")){install.packages("furrr")}; library(furrr)
if(!require("VennDiagram")){install.packages("VennDiagram")}; library(VennDiagram)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("plotly")){install.packages("plotly")}; library(plotly)

options(max.print=999999)

################################################
## ID Search
################################################
#ラベルヒット、別名ヒット、インスタンスヒット、クラス関係ヒットとかも追加
#EntityName="データサイエンス"; lang=1; Message=T; FilterRegex=F
#EntityName="重合体"; lang=1; Message=T; FilterRegex=F
#EntityName="重合体"; lang=1; Message=T; FilterRegex=F; DirSave=T; Dir="R01_Results"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#EntityName="3R"

agCount_Label_Num_DirSave <- function(EntityName2="データサイエンス"){
agCount_Label_Num(EntityName=EntityName2, Message=F, FilterRegex=F, 
                  DirSave=T, Dir="R01_Results_test", 
                  EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj")
}

agCount_Label_Num <- function(EntityName="データサイエンス", 
                              Message=T, FilterRegex=F, 
                              DirSave=F, Dir="R01_Results",
                              FROM= "From <http://wikidata_nearly_full_201127> ",
                              EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}else{}

LABEL <- EntityName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){ rdfs.l <- "ja"} else {rdfs.l <- "en"}
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
PREFIX wikibase: <http://wikiba.se/ontology#>
'

if(FilterRegex){
LAB00 <- paste('?subject rdfs:label ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")  
LAB01 <- paste('?subject skos:altLabel ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")  
}else{
LAB00 <- paste('?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '. ', sep="")
LAB01 <- paste('?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '. ', sep="")
}

Query01 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_Label)', ' ',
FROM, ' ', 
'WHERE {',LAB00,'}', sep="")
if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

A <- try(SPA01 <- SPARQL(url=EndPoint, query=paste(Prefix, Query01))$results, silent = T)
if(class(A) == "try-error"){SPA01 <- 0}else{}

Query02 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_AltLabel)', ' ',
FROM, ' ', 
'WHERE {',LAB01, '}', sep="")
A <- try(SPA02 <- SPARQL(url=EndPoint, query=paste(Prefix, Query02))$results, silent = T)
if(class(A) == "try-error"){SPA02 <- 0}else{}

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)
WHERE {',LAB00, ' ?subject wdt:P279 ?parentClass. }', sep="")
A <- try(SPA03A <- SPARQL(url=EndPoint, query=paste(Prefix, Query03A))$results, silent = T)
if(class(A) == "try-error"){SPA03A <- 0}else{}

Query03B <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_altLabel)', ' ',
FROM, ' ', 
'WHERE {',LAB01,' ?subject wdt:P279 ?parentClass. }', sep="")
A <- try(SPA03B <- SPARQL(url=EndPoint, query=paste(Prefix, Query03B))$results, silent = T)
if(class(A) == "try-error"){SPA03B <- 0}else{}

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)', ' ',
FROM, ' ', 
'WHERE {',LAB00,'
?childClass wdt:P279 ?subject.}', sep="")
A <- try(SPA04A <- SPARQL(url=EndPoint, query=paste(Prefix, Query04A))$results, silent = T)
if(class(A) == "try-error"){SPA04A <- 0}else{}

Query04B <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_altLabel)', ' ',
FROM, ' ', 
'WHERE {',LAB01,'
?childClass wdt:P279 ?subject. }', sep="")
A <- try(SPA04B <- SPARQL(url=EndPoint, query=paste(Prefix, Query04B))$results, silent = T)
if(class(A) == "try-error"){SPA04B <- 0}else{}

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)', ' ',
FROM, ' ', 
'WHERE {',LAB00,'
?instance wdt:P31 ?subject. }', sep="")
A <- try(SPA05A <- SPARQL(url=EndPoint, query=paste(Prefix, Query05A))$results, silent = T)
if(class(A) == "try-error"){SPA05A <- 0}else{}

Query05B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_altLabel)', ' ',
FROM, ' ', 
'WHERE {',LAB01,'
?instance wdt:P31 ?subject. }', sep="")
A <- try(SPA05B <- SPARQL(url=EndPoint, query=paste(Prefix, Query05B))$results, silent = T)
if(class(A) == "try-error"){SPA05B <- 0}else{}

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)', ' ',
FROM, ' ', 
'WHERE {',LAB00,'
?subject wdt:P31 ?instance. }', sep="")
A <- try(SPA06A <- SPARQL(url=EndPoint, query=paste(Prefix, Query06A))$results, silent = T)
if(class(A) == "try-error"){SPA06A <- 0}else{}

Query06B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_altLabel)', ' ',
FROM, ' ', 
'WHERE {',LAB01,'
?subject wdt:P31 ?instance. }', sep="")
A <- try(SPA06B <- SPARQL(url=EndPoint, query=paste(Prefix, Query06B))$results, silent = T)
if(class(A) == "try-error"){SPA06B <- 0}else{}

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
                  Count_As_Label=as.numeric(SPA01), 
                  Count_As_AltLabel=as.numeric(SPA02), 
                  Count_Of_ParentClass_Label=as.numeric(SPA03A), 
                  Count_Of_ParentClass_altLabel=as.numeric(SPA03B), 
                  Count_Of_ChildClass_Label=as.numeric(SPA04A), 
                  Count_Of_ChildClass_altLabel=as.numeric(SPA04B), 
                  Count_InstanceOf_Label=as.numeric(SPA06A), 
                  Count_InstanceOf_altLabel=as.numeric(SPA06B), 
                  Count_Has_Instance_Label=as.numeric(SPA05A), 
                  Count_Has_Instance_altLabel=as.numeric(SPA05B),
                  stringsAsFactors=F)

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
try(saveRDS(SPA, file = paste(Dir, "/", LABEL00, ".Rdata", sep=""), compress = TRUE), silent = T)
}else{}

return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################################################
################################################################################################################
#QID_Name="wd:Q424799"; Message=F

agCount_QID_Num <- function(QID_Name="wd:Q2374463", Message=F,
                            FROM ="From <http://wikidata_nearly_full_201127> ",
                            EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
LABEL <- QID_Name

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
PREFIX wikibase: <http://wikiba.se/ontology#>
'

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass)', ' ',
FROM, ' ', 
'WHERE { ', 
LABEL, ' wdt:P279 ?parentClass.
}', sep="")

A <- try(SPA03A <- SPARQL(url=EndPoint, query=paste(Prefix, Query03A))$results, silent = T)
if(class(A) == "try-error"){SPA03A <- 0}else{}

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass)', ' ',
FROM, ' ', 
'WHERE {
?childClass wdt:P279 ', LABEL, '.
}', sep="")
A <- try(SPA04A <- SPARQL(url=EndPoint, query=paste(Prefix, Query04A))$results, silent = T)
if(class(A) == "try-error"){SPA04A <- 0}else{}

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance)', ' ',
FROM, ' ', 
'WHERE {
?instance wdt:P31 ', LABEL, '.
}', sep="")
A <- try(SPA05A <- SPARQL(url=EndPoint, query=paste(Prefix, Query05A))$results, silent = T)
if(class(A) == "try-error"){SPA05A <- 0}else{}

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf)', ' ',
FROM, ' ', 
'WHERE {',
LABEL, ' wdt:P31 ?instance.
}', sep="")
A <- try(SPA06A <- SPARQL(url=EndPoint, query=paste(Prefix, Query06A))$results, silent = T)
if(class(A) == "try-error"){SPA06A <- 0}else{}

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
                  Count_Of_ParentClass=SPA03A, 
                  Count_Of_ChildClass=SPA04A, 
                  Count_InstanceOf=SPA06A, 
                  Count_Has_Instance=SPA05A,
                  stringsAsFactors=F)

return(data.frame(SPA, stringsAsFactors = F))
}
################################################################################################################
################################################################################################################
##QID to Label
#EntityQID="wd:Q424799"; Message=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

agQIDtoLabel <- function(EntityQID="wd:Q30060700", Message=F,
                         FROM="From <http://wikidata_nearly_full_201127> ",
                         EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
LABEL <- EntityQID

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Query <-paste('
SELECT distinct ?entityNamej ?entityNamee', ' ',
FROM, ' ', 
'WHERE {
optional {', LABEL, ' rdfs:label ?entityNamej. filter(LANG(?entityNamej) = "ja")}
optional {', LABEL, ' rdfs:label ?entityNamee. filter(LANG(?entityNamee) = "en")}',
'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){return(NULL)}else{}

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

try(SPA$entityNamej <- gsub("^\"", "", SPA$entityNamej), silent = T)
try(SPA$entityNamej <- gsub("\"@ja$", "", SPA$entityNamej), silent = T)
try(SPA$entityNamee <- gsub("^\"", "", SPA$entityNamee), silent = T)
try(SPA$entityNamee <- gsub("\"@en$", "", SPA$entityNamee), silent = T)

return(data.frame(QID=LABEL, SPA, stringsAsFactors = F))
}


################################################################################################################
##EntityName="データサイエンス"; AltLabel=F; lang=1; Message=F; LabelOut=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#EntityName=Dwiki01$LABEL[6]
################################################################################################################
#EntityName="回転半径"; AltLabel=F; LabelOut=F; lang=1; Message=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#EntityName="日化辞番号"; AltLabel=F; LabelOut=F; lang=1; Message=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

agWD_Alt <- function(EntityName="CAS登録番号", AltLabel=F, LabelOut=F, lang=1, Message=F,
                     FROM="From <http://wikidata_nearly_full_201127> ",
                     EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
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

if(LabelOut){
LabOut <- "?subject rdfs:label ?subjectLabel."
Select0 <- " ?subjectLabel "
}else{
LabOut <- ""
Select0 <- ""
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
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Query <-paste('
SELECT distinct ?subject ', Select0, Select, ' ',
FROM, ' ', 
'WHERE {
optional{?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '.}
optional{?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '.}',
LabOut,
Where1,
Where2,'}', sep="")

if(Message){message(paste("Query: ", LABEL, sep=""))}else{}
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){return(NULL)}else{}

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

if(LabelOut){
}else{
SPA <- data.frame(subject=unlist(SPA), stringsAsFactors = F)
rownames(SPA) <- 1:nrow(SPA)
}

try(SPA$subject <- gsub("^<http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
try(SPA$subject <- gsub(">$", "", SPA$subject), silent = T)
try(Lab <- grep(pattern="^wd:P", SPA$subject), silent = T)

if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

if(LabelOut){
}else{
SPA <- data.frame(subject=unlist(SPA), stringsAsFactors = F)
}

if(!is.null(nrow(SPA))){
return(data.frame(SPA, stringsAsFactors = F))  
}else{
return(data.frame(subject=NA, stringsAsFactors = F))  
}
}
################################################################################################
################################################################################################
#WDname="wd:Q12159869"; Property="?p"; Object="wd:Q101352"; Count="?p"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

agCount_QID_Property_Object_v02 <- function(WDname="wd:Q12159869", Object="wd:Q101352", 
                                            Property="?p", Count="?p", GroupBy=FALSE,
                                            FROM="From <http://wikidata_nearly_full_201127> ",
                                            EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
QID <- WDname

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
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
SELECT ', GroupBy00, ' ',
FROM, ' ', 
'WHERE { ', 
QID, ' ', Prop , ' ', Object, '. } ', 
GroupBy01, sep="")

#message(paste("Query: ", QID, sep=""))
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){
SPA <- data.frame(Count=0, stringsAsFactors = F)
}else{}

if(GroupBy){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}else{}

if(exists("SPA")){}else{return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
return(data.frame(SPA, stringsAsFactors = F))
}

###########################################################################
#WDname="wd:Q12159869"; Property="wdt:P31"; Object="?o"; GroupBy=FALSE;  DISTINCT=T; PREFIX=T; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

agWD_QID_property_Object_v01 <- function(WDname="wd:Q12159869", Property="wdt:P31", 
                                         Object="?o", DISTINCT=T, PREFIX=T,
                                         FROM="From <http://wikidata_nearly_full_201127> ",
                                         EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
QID <- WDname
Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Prop <- Property
if(DISTINCT){
  Distinct00 <- paste("distinct ", Object, " ", Object, "Labelj ", Object, "Labele ", sep="")
}else{
  Distinct00 <- paste(" ", Object, " ", Object, "Labelj ", Object, "Labele ", sep="")
}

Query <-paste('SELECT ', Distinct00, ' ',
FROM, ' ', 
'WHERE { ', 
QID, ' ', Prop , ' ', Object, '. ', 
Object, ' rdfs:label ', Object, 'Labelj . filter(LANG(', Object, 'Labelj) = "ja"). ',
Object, ' rdfs:label ', Object, 'Labele . filter(LANG(', Object, 'Labele) = "en"). }', sep="")

#message(paste("Query: ", QID, sep=""))
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){
SPA <- data.frame(o=NA, oLabelj=NA, oLabele=NA, stringsAsFactors = F)
}else{}

if(PREFIX){
try(SPA[,1] <- gsub("<http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("<http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub(">", "", SPA[,1]), silent = T)
try(SPA[,2] <- stringr::str_sub(SPA[,2], start=2, end=-5), silent = T)
try(SPA[,3] <- stringr::str_sub(SPA[,3], start=2, end=-5), silent = T)
}else{}

if(exists("SPA")){}else{return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}
try(SPA$s <- QID, silent = T)
return(data.frame(SPA, stringsAsFactors = F))
}

###########################################################################
###########################################################################
#WDname="wd:Q12159869"; Property="?p"; Object="?o"; Select="?p ?propLabelj ?propLabele "; Direct="?prop wikibase:directClaim ?p . "; JA='?prop rdfs:label ?propLabelj . filter(LANG(?propLabelj) = "ja"). '; EN='?prop rdfs:label ?propLabele . filter(LANG(?propLabele) = "en"). '; DISTINCT=T; PREFIX=T; subject=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

agWD_QID_Prop_Obj_v01 <- function(WDname="wd:Q12159869", Property="?p", 
                                  Object="?o", 
                                  Select="?p ?propLabelj ?propLabele ", 
                                  Direct="?prop wikibase:directClaim ?p . ", 
                                  JA='?prop rdfs:label ?propLabelj . filter(LANG(?propLabelj) = "ja"). ',
                                  EN='?prop rdfs:label ?propLabele . filter(LANG(?propLabele) = "en"). ',
                                  DISTINCT=T, PREFIX=T, subject=F,
                                  FROM="From <http://wikidata_nearly_full_201127> ",
                                  EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){
QID <- WDname

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Prop <- Property
if(DISTINCT){
  Distinct00 <- paste("distinct ", Select, sep="")
}else{
  Distinct00 <- paste(" ", Select, sep="")
}

Query <-paste0('
SELECT ', Distinct00, ' ',
FROM, ' ', 
'WHERE { ', 
QID, ' ', Prop , ' ', Object, '. ',
Direct, JA, EN, ' } ')

#message(paste("Query: ", QID, sep=""))
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){return(NULL)}

if(PREFIX){
try(SPA[,1] <- gsub("<http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("<http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub(">", "", SPA[,1]), silent = T)
try(SPA[,2] <- stringr::str_sub(SPA[,2], start=2, end=-5), silent = T)
try(SPA[,3] <- stringr::str_sub(SPA[,3], start=2, end=-5), silent = T)
}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}

if(subject){return(data.frame(QID, SPA, stringsAsFactors = F))}

return(data.frame(SPA, stringsAsFactors = F))
}

###########################################################################
###########################################################################
#WDname="wd:Q12159869"; Property="?p"; Object="?o"; Select="?p ?propLabel"; Direct=""; lang=1; DISTINCT=T; PREFIX=T
wikiWD_QID_Prop_Obj_v01 <- function(WDname="wd:Q12159869", Property="?p", 
                                           Object="?o", Select="?p ?propLabel", 
                                           Direct="?prop wikibase:directClaim ?p . ", 
                                           lang=1, DISTINCT=T, PREFIX=T, subject=F){
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
PREFIX wikibase: <http://wikiba.se/ontology#>
'



Prop <- Property
if(DISTINCT){
  Distinct00 <- paste("distinct ", Select, sep="")
}else{
  Distinct00 <- paste(" ", Select, sep="")
}

Query <-paste('
SELECT ', Distinct00, '
WHERE { ', 
QID, ' ', Prop , ' ', Object, '. 
SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '". } ',
Direct, '} ', sep="")

#message(paste("Query: ", QID, sep=""))
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(PREFIX){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}else{}

if(exists("SPA")){}else{
return(message(paste("Perhaps No Internet Services: ", QID, sep="")))}

if(subject){try(SPA$QID <- QID, silent = T)}
return(data.frame(SPA, stringsAsFactors = F))
}

################################################################################################################
################################################################################################################
wikiWD3_Alt <- function(EntityName="データサイエンス", AltLabel=F, lang=1, Message=F){
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
PREFIX wikibase: <http://wikiba.se/ontology#>
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
################################################################################################################
################################################################################################################
################################################################################################################
#WDname="wd:Q12159869"; Property="wdt:P31"; Object="?o"; lang=1; DISTINCT=T; PREFIX=T

################################################################################################################
agSearchLabel_wiki <- function(Label="重合体", #Label="wd:Q81163" #Label="wd:Q17748162"
                        Message=T,
                        FROM="From <http://wikidata_nearly_full_201127> ",
                        EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj",
                        property="rdfs:subClassOf"){

LABELn <- Label

if(!(is.integer(grep("^wd:", as.character(LABELn))) && length(grep("^wd:", as.character(LABELn))) == 0L)){
  WD <- TRUE
}else{
  WD <- FALSE
  if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "@ja"} else {rdfs.l <- "@en"}
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
PREFIX wikibase: <http://wikiba.se/ontology#>
'

if(WD){
Query <- paste('
select distinct ?subject', ' ',
FROM, ' ', 
'where {',
LABELn, ' rdfs:label ?subject.','
}
', sep="")
}else{
Query <- paste('
select distinct ?subject', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
}
', sep="")
}

if(Message){message(paste("Query: ", LABELn, sep=""))}else{}
A <- try(res1 <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)

if(class(A) == "try-error"){}else{}

if(WD){
Query2 <- paste('
select (count(distinct ?parentClass) As ?count)', ' ',
FROM, ' ', 
'where {',
LABELn ,' ', property, '* ?parentClass .
}
', sep="")  
}else{
Query2 <- paste('
select (count(distinct ?parentClass) As ?count)', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?subject ', property, '* ?parentClass .
}
', sep="")  
}

try(res2 <- SPARQL(url=EndPoint, query=paste(Prefix, Query2))$results, silent = T)

if(WD){
Query3 <- paste('
select (count(distinct ?childClass) As ?count)', ' ',
FROM, ' ', 
'where {
?childClass',' ', property, '* ', LABELn, ' .
}', sep="")  
}else{
Query3 <- paste('
select (count(distinct ?childClass) As ?count)', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?childClass ', property, '* ?subject .
}
', sep="")
}
try(res3 <- SPARQL(url=EndPoint, query=paste(Prefix, Query3))$results, silent = T)

if(WD){
Query4 <- paste('
select distinct ?ExpandDownwardLevels', ' ',
FROM, ' ', 
'where {', 
LABELn,' <http://property/ExpandDownwardLevels/> ?ExpandDownwardLevels . 
}', sep="")

try(res4 <- SPARQL(url=EndPoint, query=paste(Prefix, Query4))$results, silent = T)
if(nrow(res4) != 0){Num <- as.numeric(res4)}else{Num <- NA}
}else{
LABELn <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)
Query4 <- paste('
select distinct ?ExpandDownwardLevels', ' ',
FROM, ' ', 
'where {', 
LABELn,' <http://property/ExpandDownwardLevels/> ?ExpandDownwardLevels . 
}', sep="")

try(res4 <- SPARQL(url=EndPoint, query=paste(Prefix, Query4))$results, silent = T)
if(nrow(res4) != 0){Num <- as.numeric(res4)}else{Num <- NA}
}

if(WD){
return(data.frame(Label=stringr::str_sub(as.character(res1), 2, -5), QID=Label, 
                  parentClassNum=as.numeric(res2), childClassNum=as.numeric(res3), 
                  ExpandDownwardLevels=Num, stringsAsFactors = F))
}else{
Dat <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)
return(data.frame(Label=Label, QID=as.character(Dat), 
                  parentClassNum=as.numeric(res2), childClassNum=as.numeric(res3), 
                  ExpandDownwardLevels=Num, stringsAsFactors = F))
}
}

########################################################
########################################################
SearchLabel <- function(Label="重合体", #Label="wd:Q81163" #Label="wd:Q17748162"
                        Message=T,
                        FROM="From <http://wikidata_nearly_full_201127> ",
                        EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj",
                        property="rdfs:subClassOf"){

LABELn <- Label

if(!(is.integer(grep("^wd:", as.character(LABELn))) && length(grep("^wd:", as.character(LABELn))) == 0L)){
  WD <- TRUE
}else{
  WD <- FALSE
  if(franc(LABELn, min_length = 1) == "jpn" | franc(LABELn, min_length = 1) == "cmn"){ rdfs.l <- "@ja"} else {rdfs.l <- "@en"}
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
PREFIX wikibase: <http://wikiba.se/ontology#>
'

if(WD){
Query <- paste('
select distinct ?subject', ' ',
FROM, ' ', 
'where {',
LABELn, ' rdfs:label ?subject.','
}
', sep="")  
}else{
Query <- paste('
select distinct ?subject', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
}
', sep="")
}

if(Message){message(paste("Query: ", LABELn, sep=""))}else{}
A <- try(res1 <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)

if(class(A) == "try-error"){}else{}

if(WD){
Query2 <- paste('
select (count(distinct ?parentClass) As ?count)', ' ',
FROM, ' ', 
'where {',
LABELn ,' ', property, '* ?parentClass .
}
', sep="")  
}else{
Query2 <- paste('
select (count(distinct ?parentClass) As ?count)', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?subject ', property, '* ?parentClass .
}
', sep="")  
}

try(res2 <- SPARQL(url=EndPoint, query=paste(Prefix, Query2))$results, silent = T)

if(WD){
Query3 <- paste('
select (count(distinct ?childClass) As ?count)', ' ',
FROM, ' ', 
'where {
?childClass',' ', property, '* ', LABELn, ' .
}', sep="")  
}else{
Query3 <- paste('
select (count(distinct ?childClass) As ?count)', ' ',
FROM, ' ', 
'where {
?subject rdfs:label "', LABELn, '"', rdfs.l, '.','
?childClass ', property, '* ?subject .
}
', sep="")
}
try(res3 <- SPARQL(url=EndPoint, query=paste(Prefix, Query3))$results, silent = T)

if(WD){
Query4 <- paste('
select distinct ?ExpandDownwardLevels', ' ',
FROM, ' ', 
'where {', 
LABELn,' <http://property/ExpandDownwardLevels/> ?ExpandDownwardLevels . 
}', sep="")

try(res4 <- SPARQL(url=EndPoint, query=paste(Prefix, Query4))$results, silent = T)
if(nrow(res4) != 0){Num <- as.numeric(res4)}else{Num <- NA}
}else{
LABELn <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)
Query4 <- paste('
select distinct ?ExpandDownwardLevels', ' ',
FROM, ' ', 
'where {', 
LABELn,' <http://property/ExpandDownwardLevels/> ?ExpandDownwardLevels . 
}', sep="")

try(res4 <- SPARQL(url=EndPoint, query=paste(Prefix, Query4))$results, silent = T)
if(nrow(res4) != 0){Num <- as.numeric(res4)}else{Num <- NA}
}

if(WD){
return(data.frame(Label=stringr::str_sub(as.character(res1), 2, -5), QID=Label, 
                  parentClassNum=as.numeric(res2), childClassNum=as.numeric(res3), 
                  ExpandDownwardLevels=Num, stringsAsFactors = F))
}else{
Dat <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)
return(data.frame(Label=Label, QID=as.character(Dat), 
                  parentClassNum=as.numeric(res2), childClassNum=as.numeric(res3), 
                  ExpandDownwardLevels=Num, stringsAsFactors = F))
}
}

################################################################################################
################################################################################################
searchClass <- function(Class="http://class/common/",
                        FROM="From <http://wikidata_nearly_full_201127> ",
                        EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Query <- paste('
select distinct ?subject', ' ',
FROM, ' ', 
'where {
?subject ?property <', Class, '> .','
}', sep="")

A <- try(res1 <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){}else{}

Dat <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)
return(as.character(Dat))
}

searchProperty <- function(Property="rdfs:label", 
                           FROM="From <http://wikidata_nearly_full_201127> ",
                           EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Query <- paste('
select distinct ?class', ' ',
FROM, ' ', 
'where {
?subject ', Property, ' ?class .','
}', sep="")

A <- try(res1 <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){}else{}

#try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
#Dat <- gsub("http://www.wikidata.org/entity/", "wd:", as.character(res1)) %>% gsub("<", "", .) %>% gsub(">", "", .)

return(as.character(res1))
}

################################################################################################