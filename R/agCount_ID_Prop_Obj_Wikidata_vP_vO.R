##' @title Counting the particular triples w/wo GroupBy Option in WIkidata.
##'
##' @param Entity_ID a vector of character; an entity URI / prefixed ID, or a variable starting with ?.
##' @param Object a vector of character; a string, a value, an entity URI / prefixed ID, or a variable starting with ?.
##' @param Val return the results as values or TRUE/FALSE.
##' If TRUE, this gives a value to hit in the data.
##' If FALSE, this gives a logical to hit in the data or not.
##'
##' @description This function is a function for users.
##' This is to count the triples with the variable of ?p in wikidata.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agCount_ID_Prop_Obj_Wikidata_vP_vO
##' @export CkeckQuery_agCount_ID_Prop_Obj_Wikidata_vP_vO
##' @examples \dontrun{
##'
##' ID <- "wd:P31"
##' agCount_ID_Prop_Obj_Wikidata_vP_vO( Entity_ID=ID, Val=TRUE )
##'
##' #show the SPARQL query
##' CkeckQuery_agCount_ID_Prop_Obj_Wikidata_vP_vO( ID )
##'
##' #Parallel processing of two variables and 4 cores using furrr package
##' library(furrr)
##' plan(multisession(workers = 4))
##' #plan()
##'
##' #Run multisession
##' IDs <- c("wd:Q81163", "wd:Q422649", "wd:Q1241898", "wd:Q706")
##' furrr::future_map(IDs, agCount_ID_Prop_Obj_Wikidata_vP_vO, .progress = TRUE)
##'
##' }

agCount_ID_Prop_Obj_Wikidata_vP_vO <- function(Entity_ID,
                                               Val=TRUE){

if(!is.logical(Val)){return(message("Warning: Not proper value of Val"))}

#Parameters
Prefix <- agGraphSearch:::PREFIX
ID <- Entity_ID
Obj <- "?o"
Prop <- "?p"
Count <- "?p"
GroupBy <- FALSE
Message <- FALSE

EndPoint <- agGraphSearch:::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch:::KzLabEndPoint_Wikidata$FROM

SPA <- agCount_ID_Property_Object(Entity_ID=ID,
                                  Property=Prop,
                                  Object=Obj,
                                  Count=Count,
                                  GroupBy=GroupBy,
                                  EndPoint=EndPoint,
                                  FROM=FROM,
                                  Message=Message)

if(Val){
return(data.frame(SPA, stringsAsFactors = F))
}else{
return(data.frame(SPA > 0, stringsAsFactors = F))
}
}

CkeckQuery_agCount_ID_Prop_Obj_Wikidata_vP_vO <- function(Entity_ID){

#Parameters
Prefix <- agGraphSearch:::PREFIX
ID <- Entity_ID
Obj <- "?o"
Prop <- "?p"
Count <- "?p"
GroupBy <- FALSE
Message <- FALSE

EndPoint <- agGraphSearch:::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch:::KzLabEndPoint_Wikidata$FROM

#GroupBy
if(GroupBy){
  GroupBy00 <- paste0('distinct ', Count , ' (count(', Count, ') as ?Count)')
  GroupBy01 <- paste0('GROUP BY ', Count)
}else{
  GroupBy00 <- paste0('(count(distinct ', Count, ') as ?Count)')
  GroupBy01 <- ""
}

#create Query
Query <-paste0("EndPoint:
", EndPoint,"
Prefix: ",
Prefix,
'```````````````````````````````````````````',
'
SELECT ', GroupBy00, '
', FROM, '
', 'WHERE {
',ID, ' ', Prop , ' ', Obj, '.
} ',
GroupBy01, '
```````````````````````````````````````````')

#message(Query)
return( message(Query) )

}


