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
##' #run
##' agWD_Alt_Wikidata( Entity_Name=Label )
##'
##' }

agWD_Alt_Wikidata <- function(Entity_Name){

#Parameters
LABEL <- Entity_Name

#EndPoint
EndPoint <- KzLabEndPoint$EndPoint
FROM <- KzLabEndPoint$FROM

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


