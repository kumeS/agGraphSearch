##' @title Searching the label corresponding to the entity url in wikidata via SPARQL.
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param lang a numeric vector.
##' if the number is 1, the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3 (default), the labels contain both English and Japanese labels.
##'
##' @param Message logical; perform an output of EntityName or not.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agQIDtoLabel_Wikidata
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' #run
##' agQIDtoLabel_Wikidata( Entity_ID = ID )
##'
##' }

agQIDtoLabel_Wikidata <- function(Entity_ID,
                                  lang=3,
                                  Message=TRUE){
#Parameters
ID <- Entity_ID

#EndPoint
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM

#main function
Res <- agIDtoLabel(Entity_ID=ID,
                   EndPoint=EndPoint,
                   FROM=FROM,
                   PropertyForLabel=1,
                   lang=lang,
                   Message=TRUE)

#change the colnames
colnames(Res) <- c("QID", "entityNamej", "entityNamee")

return(data.frame(Res, stringsAsFactors = F))

}

