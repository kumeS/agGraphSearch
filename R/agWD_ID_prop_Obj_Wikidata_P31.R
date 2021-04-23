##' @title Counting the particular triples w/wo GroupBy Option.
##'
##' @param Entity_ID a vector of character; an entity URI / prefixed ID, or a variable starting with ?.
##' @param Property a vector of character; a property URI / prefixed ID, or a variable starting with ?.
##' @param Object a vector of character; a string, a value, an entity URI / prefixed ID, or a variable starting with ?.
##' @param Count a vector of character; the corresponding variable out of Entity_ID, Property, or Object.
##' @param GroupBy logical; default is FALSE.
##' if TRUE, results were sorted by the variable of Count.
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").
##' @param Message logical; perform an output of Entity_ID or not.
##'
##'  @description This function counts the particular triples w/wo GroupBy Option.
##' This is the flexible function for the developer, and then
##' the users should use child function for counting the particular triples.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import magrittr
##' @export agCount_ID_Prop_Obj
##' @examples \dontrun{
##'
##'
##' #Tanaka (Q12159869)
##' ID <- "wd:Q12159869"
##' #family name (Q101352)
##' Obj <- "wd:Q101352"
##' Prop <- "?p"
##' Count <- "?p"
##'
##' EndPoint <- KzLabEndPoint_Wikidata$EndPoint
##' FROM <- KzLabEndPoint_Wikidata$FROM
##'
##' agCount_ID_Prop_Obj(
##'   Entity_ID=ID,
##'   Property=Prop,
##'   Object=Obj,
##'   Count=Prop,
##'   GroupBy=FALSE,
##'   EndPoint=EndPoint,
##'   FROM=FROM
##'   )
##'
##' }


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
