##' @title Searching the label corresponding to the entity url via SPARQL.
##'
##' @param Entity_Name a character vector corresponing to the entity label.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import SPARQL franc
##' @export agWD_Alt_Wikidata
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
##' agWD_Alt_Wikidata(
##'   Entity_Name=Label,
##'   EndPoint=KzLabEndPoint$EndPoint,
##'   FROM=KzLabEndPoint$FROM
##'   )
##'
##' }

agWD_Alt_Wikidata <- function(Entity_Name){


#for wikidata
try(SPA$subject <- base::gsub("^<http://www.wikidata.org/entity/", "wd:", SPA$subject), silent = T)
try(SPA$subject <- base::gsub(">$", "", SPA$subject), silent = T)
try(Lab <- base::grep(pattern="^wd:P", SPA$subject), silent = T)

if(length(Lab) != 0){
  SPA <- SPA[-Lab,]
}

if(!is.null(nrow(SPA))){
return(base::data.frame(SPA, stringsAsFactors = F))
}else{
return(base::data.frame(subject=NA, stringsAsFactors = F))
}

}


