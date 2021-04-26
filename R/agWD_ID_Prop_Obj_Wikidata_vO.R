##' @title Counting the particular triples w/wo GroupBy Option.
##'
##' @param Entity_ID a vector of character; an entity URI / prefixed ID, or a variable starting with ?.
##' @param Property a vector of character; a property URI / prefixed ID, or a variable starting with ?.
##'
##' @param lang a numeric vector.
##' if the number is 1, the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3 (default), the labels contain both English and Japanese labels.
##'
##' @description This function counts the particular triples w/wo GroupBy Option.
##' This is the flexible function for the developer, and then
##' the users should use child function for counting the particular triples.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agWD_ID_Prop_Obj_Wikidata_vO
##' @export CkeckQuery_agWD_ID_Prop_Obj_Wikidata_vO
##' @examples \dontrun{
##'
##' #Tanaka (Q12159869)
##' ID <- "wd:Q12159869"
##'
##' #variables
##' Prop <- "wdt:P31"
##'
##' agWD_ID_Prop_Obj_Wikidata_vO(
##'   Entity_ID=ID,
##'   Property=Prop
##'   )
##'
##' }

agWD_ID_Prop_Obj_Wikidata_vO <- function(Entity_ID,
                                         Property,
                                         lang=3){

#Parameters
ID <- Entity_ID
Prefix <- agGraphSearch::PREFIX
Prop <- Property
Obj <- "?o"
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM
DISTINCT <- TRUE
PREFIX_uri <- TRUE
Message <- FALSE

SPA <- agWD_ID_Prop_Obj(Entity_ID=ID,
                        Property=Prop,
                        Object=Obj,
                        DISTINCT=DISTINCT,
                        PREFIX_uri=PREFIX_uri,
                        lang=lang,
                        EndPoint=EndPoint,
                        FROM=FROM,
                        Message=Message)

if(!exists("SPA")){return(message(paste("Perhaps No Internet Services: ", ID, sep="")))}

return(data.frame(SPA, stringsAsFactors = F))

}

CkeckQuery_agWD_ID_Prop_Obj_Wikidata_vO <- function(Entity_ID,
                                                    Property){
ID <- Entity_ID
Prefix <- agGraphSearch::PREFIX
Prop <- Property
Obj <- "?o"
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM
DISTINCT <- TRUE

if(DISTINCT){
  Distinct00 <- paste("distinct ", Obj, " ", Obj, "Labelj ", Obj, "Labele ", sep="")
}else{
  Distinct00 <- paste(" ", Obj, " ", Obj, "Labelj ", Obj, "Labele ", sep="")
}

Query00 <- paste('SELECT ', Distinct00, '
', FROM, '
', 'WHERE {
', ID, ' ', Prop , ' ', Obj, ' .
', Obj, ' rdfs:label ', Obj, 'Labelj . filter(LANG(', Obj, 'Labelj) = "ja").
', Obj, ' rdfs:label ', Obj, 'Labele . filter(LANG(', Obj, 'Labele) = "en").
}', sep="")

#create Query
Query <-paste0("EndPoint:
", EndPoint,"
Prefix: ",
Prefix,
'```````````````````````````````````````````',
'
', Query00, '
```````````````````````````````````````````')

#message(Query)
return( message(Query) )

}




