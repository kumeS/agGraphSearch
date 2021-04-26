##' @title Searching the labels in the wikidata endopoint via SPARQL.
##'
##' @param Entity_Name a character vector corresponing to the entity label.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agWD_Alt_Wikidata
##' @examples \dontrun{
##'
##' #CAS Registry Number (wikidata prefix URI: wd:Q102507)
##' Label <- "CAS Registry Number"
##'
##' #run SPARQL
##' agWD_Alt_Wikidata( Entity_Name=Label )
##'
##' #show the SPARQL query
##' CkeckQuery_agWD_Alt_Wikidata( Entity_Name=Label )
##'
##'
##' }

agWD_Alt_Wikidata <- function(Entity_Name){

#Parameters
LABEL <- Entity_Name

#EndPoint
EndPoint <- agGraphSearch:::KzLabEndPoint$EndPoint
FROM <- agGraphSearch:::KzLabEndPoint$FROM

SPA <- agWD_Alt(Entity_Name=LABEL,
                EndPoint=EndPoint,
                FROM=FROM,
                AltLabel=FALSE,
                Property=1,
                lang=1,
                Message=FALSE)

#for wikidata
try(SPA$subject <- base::gsub("^<http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
try(SPA$subject <- base::gsub(">$", "", SPA$subject), silent = T)
try(Lab <- base::grep(pattern="^wd:P", SPA$subject), silent = T)

if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

if(!is.null(nrow(SPA))){
return(base::data.frame(subject=SPA, stringsAsFactors = F))
}else{
return(base::data.frame(subject=NA, stringsAsFactors = F))
}
}


CkeckQuery_agWD_Alt_Wikidata <- function(Entity_Name){

#Parameters
Prefix <- agGraphSearch:::PREFIX
LABEL <- Entity_Name

AltLabel=FALSE
Property=1
lang=1
Message=FALSE
LabelOut=FALSE

EndPoint <- agGraphSearch:::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch:::KzLabEndPoint_Wikidata$FROM

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

switch(base::as.character(Property),
      "1" = p <- c("rdfs:label", "skos:altLabel"),
      return(message("Warning: Not proper value of Property"))
)

Query01 <-paste('
SELECT distinct ?subject ', Select0, Select, '
',
FROM, '
', 'WHERE {
optional{ ?subject ', p[1], ' \"', LABEL, '\"@', rdfs.l, '. }
optional{ ?subject ', p[2], ' \"', LABEL, '\"@', rdfs.l, '. }
', LabOut,
Where1,
Where2,'}
', sep="")

#create Query
Query <-paste0("EndPoint:
", EndPoint,
'
', "Prefix:" , Prefix,
'```````````````````````````````````````````',
Query01,
'```````````````````````````````````````````')

message(Query)

return( message(Query) )

}


