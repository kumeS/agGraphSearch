##' @title Count of concept relations using URI/ID via SPARQL.
##'
##' @param ID_Name a character vector corresponing to the entity ID.
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").
##' @param Property a list of two character vectors.
##' The first element of list contain the first property ID.
##' The second element of list contain the second property ID.
##' @param Message logical; perform an output of EntityName or not.
##' @param DirSave logical; save the results in the Dir path or not
##' @param Dir a folder path for output files.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##' the specific functions for each endpoint were also prepared.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import franc SPARQL
##' @export agCount_ID_Num
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' print(KzLabEndPoint)
##' print(wikidataClassProperty)
##'
##' #run
##' agCount_ID_Num(
##'   ID_Name=ID,
##'   EndPoint=KzLabEndPoint$EndPoint,
##'   FROM=KzLabEndPoint$FROM,
##'   Property=wikidataClassProperty)
##'
##' }

agCount_ID_Num <- function(ID_Name,
                           EndPoint,
                           FROM ="",
                           Property,
                           Message=TRUE,
                           DirSave=FALSE,
                           Dir="R01_Results"){
ID <- ID_Name

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

if(Message){message(paste("Query: ", ID, sep=""))}

Query03A <-paste('
SELECT  (count(distinct ?parentClass) as ?Count_Of_ParentClass)', ' ',
FROM, ' ',
'WHERE { ',
ID, ' ', Property[[1]], ' ?parentClass.
}', sep="")

library(SPARQL)
A <- try(SPA03A <- SPARQL(url=EndPoint, query=paste(Prefix, Query03A))$results, silent = T)
if(class(A) == "try-error"){SPA03A <- 0}

Query04A <-paste('
SELECT  (count(distinct ?childClass) as ?Count_Of_ChildClass)', ' ',
FROM, ' ',
'WHERE {
?childClass ', Property[[1]], ' ', ID, '.
}', sep="")
A <- try(SPA04A <- SPARQL(url=EndPoint, query=paste(Prefix, Query04A))$results, silent = T)
if(class(A) == "try-error"){SPA04A <- 0}else{}

Query05A <-paste('
SELECT  (count(distinct ?instance) as ?Count_Has_Instance)', ' ',
FROM, ' ',
'WHERE {
?instance ', Property[[2]], ' ', ID, '.
}', sep="")
A <- try(SPA05A <- SPARQL(url=EndPoint, query=paste(Prefix, Query05A))$results, silent = T)
if(class(A) == "try-error"){SPA05A <- 0}else{}

Query06A <-paste('
SELECT  (count(distinct ?instance) as ?Count_InstanceOf)', ' ',
FROM, ' ',
'WHERE {',
ID, ' ', Property[[2]], ' ?instance.
}', sep="")
A <- try(SPA06A <- SPARQL(url=EndPoint, query=paste(Prefix, Query06A))$results, silent = T)
if(class(A) == "try-error"){SPA06A <- 0}else{}

#Property[[1]]
SPA03A.SPA04A <- as.numeric(SPA03A) + as.numeric(SPA04A)
#Property[[2]]
SPA06A.SPA05A <- as.numeric(SPA06A) + as.numeric(SPA05A)

#up
SPA03A.SPA06A <- as.numeric(SPA03A) + as.numeric(SPA06A)
#down
SPA04A.SPA05A <- as.numeric(SPA04A) + as.numeric(SPA05A)

#ALL
SPA03A.SPA04A.SPA06A.SPA05A <- SPA03A.SPA04A + SPA06A.SPA05A

#create data.frame
SPA <- data.frame(ID=ID,
                  Hit_All=SPA03A.SPA04A.SPA06A.SPA05A,
                  Hit_up_All=SPA03A.SPA06A,
                  Hit_down_All=SPA04A.SPA05A,
                  Hit_p1_all=SPA03A.SPA04A,
                  Hit_p2_all=SPA06A.SPA05A,
                  Count_p1_up=as.numeric(SPA03A),
                  Count_p1_down=as.numeric(SPA04A),
                  Count_p2_up=as.numeric(SPA06A),
                  Count_p2_down=as.numeric(SPA05A),
                  stringsAsFactors=F)

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(ID)), silent = T)
try(saveRDS(SPA, file = paste(Dir, "/", LABEL00, ".Rdata", sep=""), compress = TRUE), silent = T)
}

return(data.frame(SPA, stringsAsFactors = F))

}



