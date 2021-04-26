##' @title Counting the particular triples w/wo GroupBy Option.
##'
##' @param Entity_ID a vector of character; an entity URI / prefixed ID, or a variable starting with ?.
##' @param Property a vector of character; a property URI / prefixed ID, or a variable starting with ?.
##' @param Object a vector of character; a string, a value, an entity URI / prefixed ID, or a variable starting with ?.
##' @param DISTINCT a logical; use Distinct in the section of SELECT in SPARQL
##' @param PREFIX_uri a logical; use prefix url or not.
##'
##' @param lang a numeric vector.
##' if the number is 1 (default), the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3, the labels contain both English and Japanese labels.
##'
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").
##' @param Message logical; perform an output of Entity_ID or not.
##'
##' @description This function counts the particular triples w/wo GroupBy Option.
##' This is the flexible function for the developer, and then
##' the users should use child function for counting the particular triples.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agWD_ID_Prop_Obj
##' @examples \dontrun{
##'
##' #Tanaka (Q12159869)
##' ID <- "wd:Q12159869"
##'
##' #variables
##' Prop <- "wdt:P31"
##' Obj <- "?o"
##'
##' EndPoint <- KzLabEndPoint_Wikidata$EndPoint
##' FROM <- KzLabEndPoint_Wikidata$FROM
##'
##' agWD_ID_Prop_Obj(
##'   Entity_ID=ID,
##'   Property=Prop,
##'   Object=Obj,
##'   DISTINCT=TRUE,
##'   PREFIX=TRUE,
##'   EndPoint=EndPoint,
##'   FROM=FROM
##'   )
##'
##' }


agWD_ID_Prop_Obj <- function(Entity_ID,
                             Property,
                             Object,
                             DISTINCT=TRUE,
                             PREFIX_uri=TRUE,
                             lang=1,
                             EndPoint,
                             FROM,
                             Message=TRUE){

#Parameters
ID <- Entity_ID
Prefix <- agGraphSearch:::PREFIX
Prop <- Property
Obj <- Object

if(DISTINCT){
  Distinct00 <- paste("distinct ", Obj, " ", Obj, "Labelj ", Obj, "Labele ", sep="")
}else{
  Distinct00 <- paste(" ", Obj, " ", Obj, "Labelj ", Obj, "Labele ", sep="")
}

Query <-paste('SELECT ', Distinct00, ' ',
FROM, ' ',
'WHERE { ',
ID, ' ', Prop , ' ', Obj, '. ',
Obj, ' rdfs:label ', Obj, 'Labelj . filter(LANG(', Obj, 'Labelj) = "ja"). ',
Obj, ' rdfs:label ', Obj, 'Labele . filter(LANG(', Obj, 'Labele) = "en"). }', sep="")

if(Message){ message(paste("Query: ", ID, sep=""))}

A <- try(SPA <- SPARQL::SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){
SPA <- data.frame(p=Prop, o=NA, oLabelj=NA, oLabele=NA, stringsAsFactors = F)
}

if(lang == 1){ SPA <- SPA[,-2] }
if(lang == 2){ SPA <- SPA[,-3] }
if(lang == 3){ SPA <- SPA }

if(PREFIX_uri){

for(n in 1:dim(agGraphSearch::URI2Prefix)[1]){
#n <- 1
try(SPA[,1] <- gsub(paste0("^", agGraphSearch::URI2Prefix[n,2]), agGraphSearch::URI2Prefix[n,1], SPA[,1]), silent = T)
try(SPA[,1] <- gsub(paste0(agGraphSearch::URI2Prefix[n,3], "$"), "", SPA[,1]), silent = T)
}

if(lang == 1){ try(SPA[,2] <- stringr::str_sub(SPA[,2], start=2, end=-5), silent = T) }
if(lang == 2){ try(SPA[,2] <- stringr::str_sub(SPA[,2], start=2, end=-5), silent = T) }
if(lang == 3){
  try(SPA[,2] <- stringr::str_sub(SPA[,2], start=2, end=-5), silent = T)
  try(SPA[,3] <- stringr::str_sub(SPA[,3], start=2, end=-5), silent = T)
}
}

if(!exists("SPA")){return(message(paste("Perhaps No Internet Services: ", ID, sep="")))}

return(data.frame(s=ID, p=Prop, SPA, stringsAsFactors = F))

}
