##' @title Count triples from a label query for Wikidata local endpoint via SPARQL.
##'
##' @param Entity_Name a character vector. The string was
##' automatically judged to be Japanese (@ja) or English (@en)
##' @param Message logical; perform an output of Entity_Name or not.
##' @description
##' This function allow to count labels and class relations for
##' wikidata local endpoint at OECU via SPARQL.
##' For parameters of EndPoint and graph id, the variable of KzLabEndPoint_Wikidata is used.
##' For parameters of properties, the variable of wikidataClassProperty is used.
##' @return data.frame
##' @author Satoshi Kume
##' @export agCount_Label_Num_Wikidata_P279_P31
##' @export CkeckQuery_agCount_Label_Num_Wikidata_P279_P31
##'
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' Label <- "polymer"
##'
##' print(KzLabEndPoint_Wikidata)
##' print(wikidataClassProperty)
##'
##' #run SPARQL
##' agCount_Label_Num_Wikidata_P279_P31(
##'   Entity_Name=Label,
##'   Message=TRUE
##'   )
##'
##' #show the SPARQL query
##' CkeckQuery_agCount_Label_Num_Wikidata_P279_P31(
##'   Entity_Name=Label,
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

agCount_Label_Num_Wikidata_P279_P31 <- function(Entity_Name,
                                                Message=FALSE){

#Parameter set
#Labels
LABEL <- Entity_Name

#EndPoint
EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM

#Property
Property <- agGraphSearch::wikidataClassProperty

#Others
FilterRegex=FALSE
DirSave=TRUE
Dir="R01_Results"

#Script
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

SPA <- agCount_Label_Num(Entity_Name=LABEL,
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


CkeckQuery_agCount_Label_Num_Wikidata_P279_P31 <- function(Entity_Name){

#Parameters
Prefix <- agGraphSearch::PREFIX
LABEL <- Entity_Name
FilterRegex <- FALSE
Property <- agGraphSearch::wikidataClassProperty

EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM

if(franc::franc(LABEL, min_length = 1) == "jpn" | franc::franc(LABEL, min_length = 1) == "cmn"){rdfs.l <- "ja" } else { rdfs.l <- "en" }

if(FilterRegex){
LAB00 <- paste('?subject rdfs:label ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")
LAB01 <- paste('?subject skos:altLabel ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ', sep="")
}else{
LAB00 <- paste('?subject rdfs:label \"', LABEL, '\"@', rdfs.l, '. ', sep="")
LAB01 <- paste('?subject skos:altLabel \"', LABEL, '\"@', rdfs.l, '. ', sep="")
}

if(franc::franc(LABEL, min_length = 1) == "jpn" | franc::franc(LABEL, min_length = 1) == "cmn"){rdfs.l <- "ja" } else { rdfs.l <- "en" }

Query01 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_Label)', '
', FROM, '
', 'WHERE {
', LAB00,'
}', sep="")

Query02 <-paste('
SELECT (count(distinct ?subject) as ?Count_As_AltLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
}', sep="")

Query03A <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?subject ', Property[[1]], ' ?parentClass.
}', sep="")

Query03B <-paste('
SELECT  (count(distinct ?parentClass ) as ?Count_Of_ParentClass_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?subject ', Property[[1]], ' ?parentClass.
}', sep="")

Query04A <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?childClass ', Property[[1]], ' ?subject.
}', sep="")

Query04B <-paste('
SELECT  (count(distinct ?childClass ) as ?Count_Of_ChildClass_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?childClass ', Property[[1]], ' ?subject.
}', sep="")

Query05A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?instance ', Property[[2]], ' ?subject.
}', sep="")

Query05B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_Has_Instance_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?instance ', Property[[2]], ' ?subject.
}', sep="")

Query06A <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?subject ', Property[[2]], ' ?instance.
}', sep="")

Query06B <-paste('
SELECT  (count(distinct ?instance ) as ?Count_InstanceOf_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?subject ', Property[[2]], ' ?instance.
}', sep="")

#create Query
Query <-paste0("EndPoint:
", EndPoint,
'
', "Prefix:" , Prefix,
'```````````````````````````````````````````',
'
### 001 ###
',
'```````````````````````````````````````````',
Query01, '
',
'```````````````````````````````````````````',
'
### 002 ###
',
'```````````````````````````````````````````',
Query02, '
',
'```````````````````````````````````````````',
'
### 003 ###
',
'```````````````````````````````````````````',
Query03A, '
',
'```````````````````````````````````````````',
Query03B, '
',
'```````````````````````````````````````````',
'
### 004 ###
',
'```````````````````````````````````````````',
Query04A, '
',
'```````````````````````````````````````````',
Query04B, '
',
'```````````````````````````````````````````',
'
### 005 ###
',
'```````````````````````````````````````````',
Query06A, '
',
'```````````````````````````````````````````',
Query06B, '
',
'```````````````````````````````````````````',
'
### 006 ###
',
'```````````````````````````````````````````',
Query05A, '
',
'```````````````````````````````````````````',
Query05B, '
',
'```````````````````````````````````````````')

#message(Query)
return( message(Query) )

}



