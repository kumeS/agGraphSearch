##' @title Count triples from a label query via SPARQL for Mesh RDF.
##' @param Entity_Name a character vector.
##' The string should be in English (@en)
##' @param Dir  a folder path for output files.
##' @description
##' This function allow to count labels and class relations for
##' Mesh RDF endpoint via SPARQL.
##' For parameters of EndPoint and graph id, the variable of EndPoint_MeshRDF is used.
##' For parameters of properties, the variable of MeshRDF_ClassProperty is used.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agCount_Label_Num_MeshRDF
##' @importFrom SPARQL SPARQL
##'
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' Entity_Name <- "Polymers"
##' Dir <- "Mesh_test"
##'
##' #run SPARQL
##' agCount_Label_Num_MeshRDF(
##'   Entity_Name=Entity_Name,
##'   Dir=Dir
##'   )
##'
##' #show the SPARQL query
##' CkeckQuery_agCount_Label_Num_MeshRDF(
##'   Entity_Name=Entity_Name,
##'   Dir=Dir
##'   )
##'
##' #Parallel processing of 4 cores using furrr package
##' library(furrr)
##' plan(multisession(workers = 4))
##' #plan()
##'
##' #prepare a vector of labels
##' Labs <- c("polymer", "protein", "Pteridophyta", "material substance")
##'
##' #run multisession
##' results <- future_map(Labs, agCount_Label_Num_MeshRDF, .progress=T)
##' results
##'
##' }
##'


agCount_Label_Num_MeshRDF <- function(Entity_Name,
                                      Dir,
                                      FilterRegex=TRUE){
#Parameter set
#Labels
LABEL <- Entity_Name

#EndPoint
EndPoint=agGraphSearch::KzLabEndPoint_MESH$EndPoint
FROM=agGraphSearch::KzLabEndPoint_MESH$FROM

#Property
#Property=agGraphSearch::meshClassProperty

#Others
FilterRegex=FilterRegex
DirSave=TRUE
Dir=Dir
Message=FALSE
Switch="Mesh"

#Script
if(!grepl("^http", EndPoint)){return(message("No EndPoint URL"))}
if(DirSave){if(!dir.exists(Dir)){dir.create(Dir)}}

SPA <-agCount_Label_Num(Entity_Name=LABEL,
                         EndPoint=EndPoint,
                         FROM = FROM,
                         Message=Message,
                         FilterRegex=FilterRegex,
                         DirSave=FALSE,
                         Dir=Dir,
                         Switch=Switch)

#colnames(SPA)
#change the colnames
SPA <- SPA[,c("LABEL", "Hit_Label", "Hit_All", "Hit_up_All", "Hit_down_All",
              "Count_As_Label", "Count_As_AltLabel", "Count_p1up_Label",
              "Count_p1up_altLabel", "Count_p1down_Label", "Count_p1down_altLabel")]

colnames(SPA) <- c("LABEL",
                   "Hit_Label",
                   "Hit_ALL",
                   "Hit_upClass_All",
                   "Hit_downClass_All",
                   "Count_Of_Label",
                   "Count_Of_AltLabel",
                   "Count_Of_broaderDescriptor_ParentClass_Label",
                   "Count_Of_broaderDescriptor_ChildClass_Label",
                   "Count_Of_broaderDescriptor_ChildClass_Label",
                   "Count_Of_broaderDescriptor_ChildClass_altLabel")

if(DirSave){
try(LABEL00 <- gsub("/", "_", as.character(LABEL)), silent = T)
try(readr::write_excel_csv(SPA, file = paste0(Dir, "/", LABEL00, ".csv"), col_names=TRUE, append = FALSE), silent = T)
}

return(data.frame(SPA, stringsAsFactors = F))

}

##' @title View the SPARQL query of agCount_Label_Num_MeshRDF.
##'
##' @param Entity_Name a character vector.
##' The string should be in English (@en).
##' @return message
##' @author Satoshi Kume
##' @export CkeckQuery_agCount_Label_Num_MeshRDF
##' @seealso agCount_Label_Num_MeshRDF

CkeckQuery_agCount_Label_Num_MeshRDF <- function( Entity_Name ){

#Parameters
LABEL <- Entity_Name
FilterRegex <- FALSE
EndPoint <- agGraphSearch::EndPoint_MeshRDF$EndPoint
FROM <- agGraphSearch::EndPoint_MeshRDF$FROM
######################################################################
######################################################################
#Switch
Switch <- "Mesh"

#Switch section
switch (Switch,
  "wikidata" = Prefix <- agGraphSearch::PREFIX,
  "Mesh" = Prefix <- agGraphSearch::PREFIX_Mesh,
)
#Prefix <- gsub("\n", " ", Prefix)

switch (Switch,
  "wikidata" = LabelProperty <- LabelProperty01,
  "Mesh" = LabelProperty <- LabelProperty02,
)

switch (Switch,
  "wikidata" = FrancFunc <- TRUE,
  "Mesh" = FrancFunc <- FALSE,
)

switch (Switch,
  "wikidata" = Property <- agGraphSearch::wikidataClassProperty,
  "Mesh" = Property <- agGraphSearch::meshClassProperty,
)

switch (Switch,
  "wikidata" = Distinct <- "distinct",
  "Mesh" = Distinct <- "",
)

if(FrancFunc){
  if(franc::franc(LABEL, min_length = 1) == "jpn" | franc::franc(LABEL, min_length = 1) == "cmn"){rdfs.l <- "ja" } else { rdfs.l <- "en" }
}else{
  rdfs.l <- "en"
}

if(FilterRegex){
LAB00 <- paste0('?subject ', LabelProperty[[1]], ' ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ')
LAB01 <- paste0('?subject ', LabelProperty[[2]], ' ?text FILTER regex (?text, \"', LABEL, '\", \"i\"). ')
}else{
LAB00 <- paste0('?subject ', LabelProperty[[1]], ' \"', LABEL, '\"@', rdfs.l, '. ')
LAB01 <- paste0('?subject ', LabelProperty[[2]], ' \"', LABEL, '\"@', rdfs.l, '. ')
}
######################################################################
######################################################################

Query01 <-paste('
SELECT (count(', Distinct, ' ?subject) as ?Count_As_Label)', '
', FROM, '
', 'WHERE {
', LAB00,'
}', sep="")

Query02 <-paste('
SELECT (count(', Distinct, ' ?subject) as ?Count_As_AltLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
}', sep="")

Query03A <-paste('
SELECT  (count(', Distinct, ' ?parentClass ) as ?Count_Of_ParentClass_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?subject ', Property[[1]], ' ?parentClass.
}', sep="")

Query03B <-paste('
SELECT  (count(', Distinct, ' ?parentClass ) as ?Count_Of_ParentClass_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?subject ', Property[[1]], ' ?parentClass.
}', sep="")

Query04A <-paste('
SELECT  (count(', Distinct, ' ?childClass ) as ?Count_Of_ChildClass_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?childClass ', Property[[1]], ' ?subject.
}', sep="")

Query04B <-paste('
SELECT  (count(', Distinct, ' ?childClass ) as ?Count_Of_ChildClass_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?childClass ', Property[[1]], ' ?subject.
}', sep="")

if(Property[[2]] != ""){
Query05A <-paste('
SELECT  (count(', Distinct, ' ?instance ) as ?Count_Has_Instance_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?instance ', Property[[2]], ' ?subject.
}', sep="")

Query05B <-paste('
SELECT  (count(', Distinct, ' ?instance ) as ?Count_Has_Instance_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?instance ', Property[[2]], ' ?subject.
}', sep="")

Query06A <-paste('
SELECT  (count(', Distinct, ' ?instance ) as ?Count_InstanceOf_Label)', '
', FROM, '
', 'WHERE {
', LAB00, '
?subject ', Property[[2]], ' ?instance.
}', sep="")

Query06B <-paste('
SELECT  (count(', Distinct, ' ?instance ) as ?Count_InstanceOf_altLabel)', '
', FROM, '
', 'WHERE {
', LAB01, '
?subject ', Property[[2]], ' ?instance.
}', sep="")

Query07 <- paste0('### 005 ###
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

}else{

  Query05A <- ""
  Query05B <- ""
  Query06A <- ""
  Query06B <- ""

Query07 <- paste0('')

}

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
', Query07 )

#message(Query)
return( message(Query) )

}



