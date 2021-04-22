##' @title Count of concept relations in wikidata using URI/ID via SPARQL.
##'
##' @param ID_Name a character vector corresponing to the entity ID.
##' @param Message logical; perform an output of EntityName or not.
##'
##' @description this function count of concept relations of subClassOf (wdt:P279)
##' and instanceOf (wdt:P31) in wikidata RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import franc SPARQL
##' @export agCount_ID_Num_Wikidata_QID_P279_P31
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' agCount_ID_Num_Wikidata_QID_P279_P31(
##'   ID_Name=ID,
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
##' future_map(as.character(unlist(IDs)), agCount_ID_Num_Wikidata_QID_P279_P31, .progress = TRUE)
##'
##' }

agCount_ID_Num_Wikidata_QID_P279_P31 <- function(ID_Name,
                                                 Message=FALSE){

#Parameter set
#Labels
ID <- ID_Name

#EndPoint
EndPoint <- KzLabEndPoint$EndPoint
FROM <- KzLabEndPoint$FROM

#Property
Property <- wikidataClassProperty

#Others
DirSave=TRUE
Dir="R01_Results"

#Script
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

SPA <- agCount_ID_Num(ID_Name=ID,
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


