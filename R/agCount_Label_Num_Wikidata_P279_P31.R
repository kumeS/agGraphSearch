##' @title Count of labels and class relations for wikidata local endpoint at OECU via SPARQL.
##' @param EntityName a character vector. The string was
##' automatically judged to be Japanese (@ja) or English (@en)
##' @param Message logical; perform an output of EntityName or not.
##' @description
##' For parameters of EndPoint and graph id, the variable of KzLabEndPoint is used.
##' For parameters of properties, the variable of wikidataClassProperty is used.
##' @return data.frame
##' @author Satoshi Kume
##' @import readr
##' @export agCount_Label_Num_Wikidata_P279_P31
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' Label <- "polymer"
##'
##' print(KzLabEndPoint)
##' print(wikidataClassProperty)
##'
##' #run
##' agCount_Label_Num_Wikidata_P279_P31(
##'   EntityName=Label,
##'   Message=TRUE
##'   )
##'
##' ##' #Parallel processing of 4 cores using furrr package
##'
##' library(furrr)
##' plan(multisession(workers = 4))
##' #plan()
##'
##' #Run multisession
##' lab <- c("polymer", "biopolymer", "pterophyta", "calcium")
##'
##' future_map(as.character(unlist(lab)), agCount_Label_Num_Wikidata_P279_P31, .progress = TRUE)
##'
##' }

agCount_Label_Num_Wikidata_P279_P31 <- function(EntityName,
                                                Message=FALSE){

#Parameter set
#Labels
LABEL <- EntityName

#EndPoint
EndPoint <- KzLabEndPoint$EndPoint
FROM <- KzLabEndPoint$FROM

#Property
Property <- wikidataClassProperty

#Others
FilterRegex=FALSE
DirSave=TRUE
Dir="R01_Results"

#Script
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

SPA <- agCount_Label_Num(EntityName=LABEL,
                         EndPoint=EndPoint,
                         FROM = FROM,
                         Property=Property,
                         Message=Message,
                         FilterRegex=FilterRegex,
                         DirSave=FALSE,
                         Dir="")

#change the colnames
colnames(SPA) <- c("LABEL",
                   "Hit_Label",
                   "Hit_ALL",
                   "Hit_upClass_All",
                   "Hit_downClass_All",
                   "Hit_subClassOf",
                   "Hit_InstanceOf",
                   "Hit_subClassOf_ParentClass",
                   "Hit_subClassOf_ChildClass",
                   "Hit_InstanceOf_ParentClass",
                   "Hit_InstanceOf_ChildClass",
                   "Count_Of_Label",
                   "Count_Of_AltLabel",
                   "Count_Of_subClassOf_ParentClass_Label",
                   "Count_Of_subClassOf_ParentClass_altLabel",
                   "Count_Of_subClassOf_ChildClass_Label",
                   "Count_Of_subClassOf_ChildClass_altLabel",
                   "Count_Of_InstanceOf_ParentClass_Label",
                   "Count_Of_InstanceOf_ParentClass_altLabel",
                   "Count_Of_InstanceOf_ChildClass_Label",
                   "Count_Of_InstanceOf_ChildClass_altLabel")

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
try(readr::write_excel_csv(SPA, file = paste0(Dir, "/", LABEL00, ".csv"), col_names=TRUE, append = FALSE), silent = T)
}

return(data.frame(SPA, stringsAsFactors = F))

}
