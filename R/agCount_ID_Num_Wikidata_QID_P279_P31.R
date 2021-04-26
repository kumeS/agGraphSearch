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
##' @export agCount_ID_Num_Wikidata_QID_P279_P31
##' @export CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' #Run SPARQL
##' agCount_ID_Num_Wikidata_QID_P279_P31(
##'   Entity_ID=ID,
##'   Message=TRUE)
##'
##' #show the SPARQL query
##' CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31( Entity_ID=ID )
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
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM

#Property
Property <- agGraphSearch::wikidataClassProperty

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

#Property
Property <- agGraphSearch::wikidataClassProperty

EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM

Query03A <-paste('
SELECT  (count(distinct ?parentClass) as ?Count_Of_ParentClass)', '
', FROM, '
', 'WHERE {
', ID, ' ', Property[[1]], ' ?parentClass.
}', sep="")

Query04A <-paste('
SELECT  (count(distinct ?childClass) as ?Count_Of_ChildClass)', '
', FROM, '
', 'WHERE {
?childClass ', Property[[1]], ' ', ID, '.
}', sep="")

Query05A <-paste('
SELECT  (count(distinct ?instance) as ?Count_Has_Instance)', '
', FROM, '
', 'WHERE {
?instance ', Property[[2]], ' ', ID, '.
}', sep="")

Query06A <-paste('
SELECT  (count(distinct ?instance) as ?Count_InstanceOf)', '
', FROM, '
', 'WHERE {
', ID, ' ', Property[[2]], ' ?instance.
}', sep="")

#create Query
Query <-paste0("EndPoint:
", EndPoint,
'
', "Prefix:" , Prefix,
'```````````````````````````````````````````',
Query03A, '
',
'```````````````````````````````````````````',
Query04A, '
',
'```````````````````````````````````````````',
Query06A, '
',
'```````````````````````````````````````````',
Query05A, '
',
'```````````````````````````````````````````')

#message(Query)
return( message(Query) )

}

