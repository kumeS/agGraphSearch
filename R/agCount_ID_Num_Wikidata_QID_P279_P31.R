##' @title Count of concept relations in wikidata using URI/ID via SPARQL.
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param Message logical; perform an output of EntityName or not.
##'
##' @description this function count of concept relations of subClassOf (wdt:P279)
##' and instanceOf (wdt:P31) in wikidata RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import readr
##' @export agCount_ID_Num_Wikidata_QID_P279_P31
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' agCount_ID_Num_Wikidata_QID_P279_P31(
##'   Entity_ID=ID,
##'   Message=TRUE)
##'
##' #Parallel processing of 4 cores using furrr package
##' library(furrr)
##' plan(multisession(workers = 4))
##' #plan()
##'
##' #Run multisession
##' IDs <- c("wd:Q81163", "wd:Q422649", "wd:Q1241898", "wd:Q706")
##'
##' furrr::future_map2(as.character(unlist(IDs)), agCount_ID_Num_Wikidata_QID_P279_P31, .progress = TRUE)
##'
##' }

agCount_ID_Num_Wikidata_QID_P279_P31 <- function(Entity_ID,
                                                 Message=FALSE){

#Parameter set
#Labels
ID <- Entity_ID

#EndPoint
EndPoint <- KzLabEndPoint_Wikidata$EndPoint
FROM <- KzLabEndPoint_Wikidata$FROM

#Property
Property <- wikidataClassProperty

#Others
DirSave=TRUE
Dir="R01_Results"

#Script
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

SPA <- agCount_ID_Num(Entity_ID=ID,
                      EndPoint=EndPoint,
                      FROM=FROM,
                      Property=Property,
                      Message=Message,
                      DirSave=FALSE,
                      Dir="")

#change the colnames
colnames(SPA) <- c("ID",
                   "Hit_All",
                   "Hit_All_Parent",
                   "Hit_All_Child",
                   "Hit_subClassOf_all",
                   "Hit_InstanceOf_all",
                   "Count_subClassOf_Parent",
                   "Count_subClassOf_Child",
                   "Count_InstanceOf_Parent",
                   "Count_InstanceOf_Child")

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(ID)), silent = T)
try(readr::write_excel_csv(SPA, file = paste0(Dir, "/", LABEL00, ".csv"), col_names=TRUE, append = FALSE), silent = T)
}

return(data.frame(SPA, stringsAsFactors = F))

}




CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31 <- function(Entity_ID){

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
```````````````````````````````````````````
SELECT ', GroupBy00, '
', FROM, '
', 'WHERE {
',ID, ' ', Prop , ' ', Object, '.
} ',
GroupBy01, '
```````````````````````````````````````````')

return( message(Query) )

}

