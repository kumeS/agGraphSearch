##' @title Count triples from a label query via SPARQL.
##'
##' @param Entity_Name a character vector. The string was
##' automatically judged to be Japanese (@ja) or English (@en)
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").
##' @param Property a list of two character vectors.
##' The first element of list contain the first property ID.
##' The second element of list contain the second property ID.
##' @param Message logical; perform an output of Entity_Name or not.
##' @param FilterRegex do not use this option.
##' @param DirSave logical; save the results in the Dir path or not
##' @param Dir a folder path for output files.
##'
##' @description this function count labels and concept relations using labels via SPARQL.
##' this function is a general function for
##' searching the RDF data using a label information via SPARQL.
##' the specific functions for each endpoint were also prepared.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agCount_Label_Num
##' @importFrom SPARQL SPARQL
##' @importFrom RCurl getURL
##' @importFrom franc franc
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' Label <- "polymer"
##'
##' print(agGraphSearch::KzLabEndPoint_Wikidata)
##' print(agGraphSearch::wikidataClassProperty)
##'
##' #run
##' agCount_Label_Num(
##'   Entity_Name=Label,
##'   EndPoint=agGraphSearch::KzLabEndPoint_Wikidata$EndPoint,
##'   FROM=agGraphSearch::KzLabEndPoint_Wikidata$FROM,
##'   Property=agGraphSearch::wikidataClassProperty)
##' }

agCount_Label_Num <- function(Entity_Name,
                              EndPoint,
                              FROM = "",
                              Property,
                              Message=FALSE,
                              FilterRegex=FALSE,
                              DirSave=FALSE,
                              Dir="01_Out"){

if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

LABEL <- Entity_Name
if(franc::franc(LABEL, min_length = 1) == "jpn" | franc::franc(LABEL, min_length = 1) == "cmn"){rdfs.l <- "ja" } else { rdfs.l <- "en" }

Prefix <- agGraphSearch::PREFIX

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

if(Message){message(paste("Query: ", LABEL, sep=""))}
A <- try(SPA01 <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query01))$results, silent = T)
if(class(A) == "try-error"){SPA01 <- 0}

Query02 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_AltLabel)', ' ',
FROM, ' ',
'WHERE {',LAB01, '}', sep="")
A <- try(SPA02 <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query02))$results, silent = T)
if(class(A) == "try-error"){SPA02 <- 0}

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)', ' ',
FROM, ' ',
'WHERE {',LAB00, ' ?subject ', Property[[1]], ' ?parentClass. }', sep="")
A <- try(SPA03A <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query03A))$results, silent = T)
if(class(A) == "try-error"){SPA03A <- 0}

Query03B <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_altLabel)', ' ',
FROM, ' ',
'WHERE {',LAB01,' ?subject ', Property[[1]], ' ?parentClass. }', sep="")
A <- try(SPA03B <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query03B))$results, silent = T)
if(class(A) == "try-error"){SPA03B <- 0}

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)', ' ',
FROM, ' ',
'WHERE {',LAB00,'
?childClass ', Property[[1]], ' ?subject.}', sep="")
A <- try(SPA04A <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query04A))$results, silent = T)
if(class(A) == "try-error"){SPA04A <- 0}

Query04B <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_altLabel)', ' ',
FROM, ' ',
'WHERE {',LAB01,'
?childClass ', Property[[1]], ' ?subject. }', sep="")
A <- try(SPA04B <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query04B))$results, silent = T)
if(class(A) == "try-error"){SPA04B <- 0}

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)', ' ',
FROM, ' ',
'WHERE {',LAB00,'
?instance ', Property[[2]], ' ?subject. }', sep="")
A <- try(SPA05A <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query05A))$results, silent = T)
if(class(A) == "try-error"){SPA05A <- 0}

Query05B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_altLabel)', ' ',
FROM, ' ',
'WHERE {',LAB01,'
?instance ', Property[[2]], ' ?subject. }', sep="")
A <- try(SPA05B <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query05B))$results, silent = T)
if(class(A) == "try-error"){SPA05B <- 0}

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)', ' ',
FROM, ' ',
'WHERE {',LAB00,'
?subject ', Property[[2]], ' ?instance. }', sep="")
A <- try(SPA06A <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query06A))$results, silent = T)
if(class(A) == "try-error"){SPA06A <- 0}

Query06B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_altLabel)', ' ',
FROM, ' ',
'WHERE {',LAB01,'
?subject ', Property[[2]], ' ?instance. }', sep="")
A <- try(SPA06B <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query06B))$results, silent = T)
if(class(A) == "try-error"){SPA06B <- 0}

#Label
SPA01.SPA02 <- as.numeric(SPA01) + as.numeric(SPA02)
#Property[[1]]
SPA03A.SPA03B <- as.numeric(SPA03A) + as.numeric(SPA03B)
SPA04A.SPA04B <- as.numeric(SPA04A) + as.numeric(SPA04B)
SPA03A.SPA03B.SPA04A.SPA04B <- SPA03A.SPA03B + SPA04A.SPA04B
#Property[[2]]
SPA06A.SPA06B <- as.numeric(SPA06A) + as.numeric(SPA06B)
SPA05A.SPA05B <- as.numeric(SPA05A) + as.numeric(SPA05B)
SPA05A.SPA05B.SPA06A.SPA06B <- SPA05A.SPA05B + SPA06A.SPA06B
#All
SPA_Hit_ParentClass_InstanceOf <- SPA03A.SPA03B + SPA06A.SPA06B
SPA_Hit_childClass_HasInstance <- SPA04A.SPA04B + SPA05A.SPA05B
SPA_Hit_ALL <- SPA03A.SPA03B.SPA04A.SPA04B+SPA05A.SPA05B.SPA06A.SPA06B

#make data.frame
SPA <- data.frame(LABEL=LABEL,
                  Hit_Label=SPA01.SPA02,
                  Hit_All=SPA_Hit_ALL,
                  Hit_up_All=SPA_Hit_ParentClass_InstanceOf,
                  Hit_down_All=SPA_Hit_childClass_HasInstance,
                  Hit_p1_all=SPA03A.SPA03B.SPA04A.SPA04B,
                  Hit_p2_all=SPA05A.SPA05B.SPA06A.SPA06B,
                  Hit_p1_up=SPA03A.SPA03B,
                  Hit_p1_down=SPA04A.SPA04B,
                  Hit_p2_up=SPA06A.SPA06B,
                  Hit_p2_down=SPA05A.SPA05B,
                  Count_As_Label=as.numeric(SPA01),
                  Count_As_AltLabel=as.numeric(SPA02),
                  Count_p1up_Label=as.numeric(SPA03A),
                  Count_p1up_altLabel=as.numeric(SPA03B),
                  Count_p1down_Label=as.numeric(SPA04A),
                  Count_p1down_altLabel=as.numeric(SPA04B),
                  Count_p2up_Label=as.numeric(SPA06A),
                  Count_p2up_altLabel=as.numeric(SPA06B),
                  Count_p2down_Label=as.numeric(SPA05A),
                  Count_p2down_altLabel=as.numeric(SPA05B),
                  stringsAsFactors=F)

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
try(saveRDS(SPA, file = paste(Dir, "/", LABEL00, ".Rdata", sep=""), compress = TRUE), silent = T)
}

return(data.frame(SPA, stringsAsFactors = F))

}


