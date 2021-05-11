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
##' @export agIDtoLabel_Wikidata
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' #run
##' agIDtoLabel_Wikidata( Entity_ID = ID )
##'
##' }

agIDtoLabel_Wikidata <- function(Entity_ID,
                                  lang=3,
                                  Message=FALSE){
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
                   Message=Message)

#change the colnames
colnames(Res) <- c("QID", "entityNamej", "entityNamee")

return(data.frame(Res, stringsAsFactors = F))

}


##' @title View the SPARQL query of agIDtoLabel_Wikidata
##'
##' @param Entity_Name a character vector. The string was
##' automatically judged to be Japanese (@ja) or English (@en)
##' @param lang a numeric vector.
##' if the number is 1, the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3 (default), the labels contain both English and Japanese labels.
##'
##' @return message
##' @author Satoshi Kume
##' @export CkeckQuery_agIDtoLabel_Wikidata
##' @seealso agCount_Label_Num_Wikidata_P279_P31


CkeckQuery_agIDtoLabel_Wikidata <- function(Entity_ID,
                                             lang=3){
#Parameters
ID <- Entity_ID

#EndPoint
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM
Prefix <- agGraphSearch::PREFIX
PropertyForLabel <- 1

switch(as.character(PropertyForLabel),
      "1" = Property <- "rdfs:label",
      "2" = Property <- "skos:prefLabel",
      return(message("Warning: Not proper value of PropertyForLabel"))
)

Query01 <-paste('
SELECT distinct ?entityNamej ?entityNamee', '
', FROM, '
', 'WHERE {
optional { ', ID, ' ', Property, ' ?entityNamej. filter(LANG(?entityNamej) = "ja") }
optional { ', ID, ' ', Property, ' ?entityNamee. filter(LANG(?entityNamee) = "en") }
', '}
', sep="")

#create Query
Query <- paste0("EndPoint:
", EndPoint,
'
', "Prefix:" , Prefix,
'```````````````````````````````````````````',
Query01,
'```````````````````````````````````````````')

return( message(Query) )

}
