##' @title Counting the particular triples w/wo GroupBy Option in WIkidata.
##'
##' @param Entity_ID a vector of character; an entity URI / prefixed ID, or a variable starting with ?.
##' @param Object a vector of character; a string, a value, an entity URI / prefixed ID, or a variable starting with ?.
##'
##' @description This function is a function for users.
##' This is to count the triples with the variable of ?p in wikidata.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import magrittr
##' @export agCount_QID_Property_Object_Wikidata_vP
##' @examples \dontrun{
##'
##' ID <- "wd:Q12159869"
##' Obj <- "wd:Q101352"
##'
##' agCount_QID_Property_Object_Wikidata_vP(
##'   Entity_ID=ID,
##'   Object=Obj,
##'   )
##'
##' #Parallel processing of two variables and 4 cores using furrr package
##' library(furrr)
##' plan(multisession(workers = 4))
##' #plan()
##'
##' #Run multisession
##' IDs <- c("wd:Q81163", "wd:Q422649", "wd:Q1241898", "wd:Q706")
##'
##' furrr::future_map(as.character(unlist(IDs)), agCount_ID_Num_Wikidata_QID_P279_P31, .progress = TRUE)
##'
##' }

agCount_QID_Property_Object_Wikidata_vP <- function(Entity_ID,
                                                    Object){

#Parameters
Prefix <- agGraphSearch::PREFIX
ID <- Entity_ID
Obj <- Object
Prop <- "?p"
Count <- "?p"
GroupBy <- FALSE
Message <- FALSE

EndPoint <- KzLabEndPoint_Wikidata$EndPoint
FROM <- KzLabEndPoint_Wikidata$FROM

SPA <- agCount_ID_Property_Object(Entity_ID=ID,
                                  Property=Prop,
                                  Object=Obj,
                                  Count=Count,
                                  GroupBy=GroupBy,
                                  EndPoint=EndPoint,
                                  FROM=FROM,
                                  Message=Message)

return(data.frame(SPA, stringsAsFactors = F))

}

CkeckQuery_agCount_QID_Property_Object_Wikidata_vP <- function(Entity_ID,
                                                               Object){

#Parameters
Prefix <- agGraphSearch::PREFIX
ID <- Entity_ID
Prop <- "?p"
Count <- "?p"
GroupBy <- FALSE
Message <- FALSE

EndPoint <- KzLabEndPoint_Wikidata$EndPoint
FROM <- KzLabEndPoint_Wikidata$FROM

#GroupBy
if(GroupBy){
  GroupBy00 <- paste0('distinct ', Count , ' (count(', Count, ') as ?Count)')
  GroupBy01 <- paste0('GROUP BY ', Count)
}else{
  GroupBy00 <- paste0('(count(distinct ', Count, ') as ?Count)')
  GroupBy01 <- ""
}

#create Query
Query <-paste0("EndPoint: ", EndPoint,
'
```````````````````````````````````````````',
Prefix, '
SELECT ', GroupBy00, '
', FROM, '
', 'WHERE {
',ID, ' ', Prop , ' ', Object, '.
} ',
GroupBy01, '
```````````````````````````````````````````')

return( message(Query) )

}


