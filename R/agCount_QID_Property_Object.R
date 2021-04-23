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
##' @export agCount_ID_Property_Object
##' @examples \dontrun{
##'
##' ID <- "wd:Q12159869"
##' Prop <- "?p"
##' Obj <- "wd:Q101352"
##' Count <- "?p"
##'
##' EndPoint <- KzLabEndPoint_Wikidata$EndPoint
##' FROM <- KzLabEndPoint_Wikidata$FROM
##'
##' agCount_ID_Property_Object(
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

agCount_ID_Property_Object <- function(Entity_ID,
                                       Property,
                                       Object,
                                       Count,
                                       GroupBy=FALSE,
                                       EndPoint,
                                       FROM,
                                       Message=TRUE){

#Parameters
Prefix <- agGraphSearch::PREFIX
ID <- Entity_ID
Prop <- Property

#GroupBy
if(GroupBy){
  GroupBy00 <- paste0('distinct ', Count , ' (count(', Count, ') as ?Count)')
  GroupBy01 <- paste0('GROUP BY ', Count)
}else{
  GroupBy00 <- paste0('(count(distinct ', Count, ') as ?Count)')
  GroupBy01 <- ""
}

#create Query
Query <-paste0('
SELECT ', GroupBy00, ' ',
FROM, ' ',
'WHERE { ',
ID, ' ', Prop , ' ', Object, '. } ',
GroupBy01)

if(Message){ message(paste0( "Query: ",ID)) }
A <- try(SPA <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){
SPA <- data.frame(Count=0, stringsAsFactors = F)
}

if(!exists("SPA")){
  return(message(paste0("Perhaps No Internet Services: ", ID)))
}

return(data.frame(SPA, stringsAsFactors = F))

}




