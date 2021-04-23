##' @title Counting the particular triples.
##'
##' @param A
##'
##' @description A
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
ID <- Entity_ID
Prefix <- agGraphSearch::PREFIX
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

if(GroupBy){
try(SPA[,1] <- gsub("http://www.wikidata.org/prop/direct/", "wdt:", SPA[,1]), silent = T)
try(SPA[,1] <- gsub("http://www.wikidata.org/entity/", "wd:", SPA[,1]), silent = T)
}

if(!exists("SPA")){
  return(message(paste0("Perhaps No Internet Services: ", ID)))
}

return(data.frame(SPA, stringsAsFactors = F))

}




