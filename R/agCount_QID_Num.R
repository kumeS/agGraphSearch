
######################################################
#agCount_QID_Num
######################################################
agCount_QID_Num <- function(QID_Name="wd:Q2374463", EndPoint, FROM =" ", Message=F){
LABEL <- QID_Name

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
SELECT  (count(distinct ?parentClass) as ?Count_Of_ParentClass)', ' ',
FROM, ' ',
'WHERE { ',
LABEL, ' wdt:P279 ?parentClass.
}', sep="")

A <- try(SPA03A <- SPARQL(url=EndPoint, query=paste(Prefix, Query03A))$results, silent = T)
if(class(A) == "try-error"){SPA03A <- 0}else{}

Query04A <-paste('
SELECT  (count(distinct ?childClass) as ?Count_Of_ChildClass)', ' ',
FROM, ' ',
'WHERE {
?childClass wdt:P279 ', LABEL, '.
}', sep="")
A <- try(SPA04A <- SPARQL(url=EndPoint, query=paste(Prefix, Query04A))$results, silent = T)
if(class(A) == "try-error"){SPA04A <- 0}else{}

Query05A <-paste('
SELECT  (count(distinct ?instance) as ?Count_Has_Instance)', ' ',
FROM, ' ',
'WHERE {
?instance wdt:P31 ', LABEL, '.
}', sep="")
A <- try(SPA05A <- SPARQL(url=EndPoint, query=paste(Prefix, Query05A))$results, silent = T)
if(class(A) == "try-error"){SPA05A <- 0}else{}

Query06A <-paste('
SELECT  (count(distinct ?instance) as ?Count_InstanceOf)', ' ',
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

SPA <- data.frame(ID=LABEL,
                  Hit_subClassOf_Instance=SPA03A.SPA04A+SPA06A.SPA05A,
                  Hit_subClassOf=SPA03A.SPA04A,
                  Hit_Instance=SPA06A.SPA05A,
                  Hit_ParentClass=as.numeric(SPA03A),
                  Hit_ChildClass=as.numeric(SPA04A),
                  Hit_InstanceOf=as.numeric(SPA06A),
                  Hit_Has_Instance=as.numeric(SPA05A),
                  Hit_subClassOf_InstanceOf_up=SPA03A.SPA06A,
                  Hit_subClassOf_InstanceOf_down=SPA04A.SPA05A,
                  stringsAsFactors=F)

return(data.frame(SPA, stringsAsFactors = F))
}
