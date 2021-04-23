##' @title Searching the label corresponding to the entity url via SPARQL.
##'
##' @param Entity_Name a character vector corresponing to the entity label.
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").

##' @param lang a numeric vector.
##' if the number is 1 (default), the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3, the labels contain both English and Japanese labels.
##' @param AltLabel logical; default is FALSE.
##' @param Property a numeric vector.
##' if the number is 1 (default), the properties are set to be "rdfs:label" and "skos:altLabel".
##' Still no other options in a parameter of Property.
##'
##' @param Message logical; perform an output of Entity_Name or not.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @seealso list2DF {base}
##' @author Satoshi Kume
##' @import SPARQL franc
##' @export agWD_Alt
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' Label <- "CAS Registry Number"
##'
##' print(KzLabEndPoint)
##' print(wikidataClassProperty)
##'
##' #run
##' agWD_Alt(
##'   Entity_Name=Label,
##'   EndPoint=KzLabEndPoint$EndPoint,
##'   FROM=KzLabEndPoint$FROM
##'   )
##'
##' }

agWD_Alt <- function(Entity_Name,
                     EndPoint,
                     FROM,
                     AltLabel=FALSE,
                     Property=1,
                     lang=1,
                     Message=TRUE){

#Parameters
# @param LabelOut logical; default is FALSE.
LabelOut=FALSE

LABEL <- Entity_Name
if(franc::franc(LABEL, min_length = 1) == "jpn" | franc::franc(LABEL, min_length = 1) == "cmn"){
   rdfs.l <- "ja" } else {rdfs.l <- "en"}

if(lang == 1){
    lang1 <- "en"
    Select <- "?alsoKnownAse"
    Where1 <- ""
    Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'
} else if(lang == 2){
    lang1 <- "ja"
    Select <- "?alsoKnownAsj"
    Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
    Where2 <- ""
} else if(lang == 3){
    lang1 <- "ja, en"
    Select <- "?alsoKnownAsj ?alsoKnownAse"
    Where1 <- 'optional{ ?subject skos:altLabel ?alsoKnownAsj . filter(LANG(?alsoKnownAsj) = "ja") }'
    Where2 <- 'optional{ ?subject skos:altLabel ?alsoKnownAse . filter(LANG(?alsoKnownAse) = "en") }'
}

if(!AltLabel){
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

Prefix <- agGraphSearch::PREFIX

switch(base::as.character(Property),
      "1" = p <- c("rdfs:label", "skos:altLabel"),
      return(message("Warning: Not proper value of Property"))
)

Query <-paste('
SELECT distinct ?subject ', Select0, Select, ' ',
FROM, ' ',
'WHERE {
optional{?subject ', p[1], ' \"', LABEL, '\"@', rdfs.l, '.}
optional{?subject ', p[2], ' \"', LABEL, '\"@', rdfs.l, '.} ',
LabOut,
Where1,
Where2,'
}', sep="")

if(Message){base::message(paste("Query: ", LABEL, sep=""))}else{}
A <- try(SPA <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){return(NULL)}

if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

if(!LabelOut){
SPA <- base::data.frame(subject=unlist(SPA), stringsAsFactors = F)
rownames(SPA) <- 1:nrow(SPA)
}

if(!is.null(nrow(SPA))){
return(base::data.frame(SPA, stringsAsFactors = F))
}else{
return(base::data.frame(subject=NA, stringsAsFactors = F))
}
}
