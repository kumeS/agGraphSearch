

######################################################
#agCount_Label_Num_DirSave
######################################################
agCount_Label_Num_DirSave <- function(EntityName, EndPoint, Dir="Results_test"){
agCount_Label_Num(EntityName=EntityName, Message=F, FilterRegex=F,
                  DirSave=T, Dir=Dir, EndPoint=EndPoint)
}

######################################################
#agCount_Label_Num
######################################################
agCount_Label_Num <- function(EntityName, EndPoint, FROM = "",
                              Message=F, FilterRegex=F,
                              DirSave=F, Dir="R01_Results"){
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}else{}

LABEL <- EntityName
if(franc(LABEL, min_length = 1) == "jpn" | franc(LABEL, min_length = 1) == "cmn"){rdfs.l <- "ja" } else { rdfs.l <- "en" }

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
                  Hit_Class_All=SPA_Hit_ParentClass_InstanceOf,
                  Hit_subClassOf_InstanceOf=SPA03A.SPA03B.SPA04A.SPA04B+SPA05A.SPA05B.SPA06A.SPA06B,
                  Hit_subClassOf=SPA03A.SPA03B.SPA04A.SPA04B,
                  Hit_InstanceOf=SPA05A.SPA05B.SPA06A.SPA06B,
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
}

return(data.frame(SPA, stringsAsFactors = F))

}
