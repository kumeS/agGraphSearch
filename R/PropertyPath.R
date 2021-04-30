##' @title Search class-related hierarchy by the property path of SPARQL query.
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param Message logical; perform an output of EntityName or not.
##'
##' @export PropertyPath
##' @export CkeckQuery_PropertyPath
##'

#Entity_ID="wd:Q180664"; PropertyPathDepth=2; Upper=TRUE; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=FALSE; JA=TRUE; EndPoint <- agGraphSearch::KzLabEndPoint_Wikidata$EndPoint; FROM <- agGraphSearch::KzLabEndPoint_Wikidata$FROM; FolferPath="03_Out"; PREFIX_uri=TRUE


PropertyPath <- function(Entity_ID,
                         PropertyPathDepth,
                         Upper=TRUE,
                         p1,
                         p2,
                         Message=FALSE,
                         JA=TRUE,
                         PREFIX_uri=TRUE,
                         FROM,
                         EndPoint,
                         FolferPath){

IDs <- Entity_ID
if(Message){message(paste(" ID:  ", IDs, "\n",
                          "Depth: ", PropertyPathDepth), sep="")}

Prefix <- agGraphSearch::PREFIX

Query <-paste0('SELECT distinct ?oj ?oe ', ' ',
               FROM, ' ',
               'WHERE {
               optional{ ', IDs, ' rdfs:label ?oj . filter(LANG(?oj) = "ja") }
               optional{ ', IDs, ' rdfs:label ?oe . filter(LANG(?oe) = "en") } }')

suppressMessages(try(SPA00 <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))

############################################################
############################################################
# Create query
############################################################
############################################################
if(Upper){
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("IDs,  p1,  '?s1', '.', 'optional{?s1 rdfs:label ?s1Label.} ' "))
}else{
c <- c(c, paste0("IDs,  p1,  '?s1', '.', ' ', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
  c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.', ' ', "))
}else{
  c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.',
                   'optional{?s", n-1," rdfs:label ?s", n-1, "Label.} optional{?s", n," rdfs:label ?s", n, "Label.} ' "))
}}}
c <- c(c, paste0("), ncol = 5, byrow = T)"))
eval(parse(text=paste(c, collapse = "")))

COL <- c("subject", "subjectLabel",
         "parentClass", "parentClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[3], paste(List00[3], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}
}else{
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("'?s1',  p1,   IDs , '.', 'optional{?s1 rdfs:label ?s1Label.} ' "))
}else{
c <- c(c, paste0("'?s1',  p2,   IDs , '.', ' ', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
  c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', ' ', "))
}else{
  c <- c(c, paste0("'?s", n, "',  p1 ,'?s", n-1, "', '.',
                   'optional{?s", n-1," rdfs:label ?s", n-1, "Label.} optional{?s", n," rdfs:label ?s", n, "Label.} ' "))
}}}
c <- c(c, paste0("), ncol = 5, byrow = T)"))
eval(parse(text=paste(c, collapse = "")))

COL <- c("subject", "subjectLabel",
         "childClass", "childClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[1], paste(List00[1], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}}
############################################################
############################################################
Query <-paste0('SELECT distinct ', PropertyPath01, ' ',
               FROM, ' ',
               'WHERE { ',  QueryPath, ' }' )

if(Message){message(paste("Query: ", IDs, sep=""))}
suppressMessages(A <- try(SPA <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))

if(class(A) == "try-error"){
DD <- data.frame(ID=Entity_ID,
                 PropertyPathDepth=PropertyPathDepth,
                 Upper=Upper,
                 p1=p1,
                 p2=p2,
                 Message=Message,
                 JA=JA,
                 FROM=FROM,
                 EndPoint=EndPoint,
                 FolferPath=FolferPath)
readr::write_excel_csv(data.frame(DD), append=T, col_names=F,
                       file= paste0(FolferPath, "/", Entity_ID, "_try-error.csv"))
return(NULL)
}

if(!exists("SPA")){return(NULL)}
if(nrow(SPA) == 0){return(NULL)}
#SPA00; SPA

if(JA){
if(any(!is.na(SPA00))){
if(any(grepl("@ja", SPA00))){
SPA01 <- SPA00[!is.na(as.character(SPA00))]
SPA01 <- SPA01[grepl("@ja", SPA01)]
try(SPA02 <- as.character(stringr::str_sub(SPA01, 2, -5)), silent = T)
}else{
SPA01 <- SPA00[!is.na(as.character(SPA00))]
SPA01 <- SPA01[grepl("@en", SPA01)]
try(SPA02 <- as.character(stringr::str_sub(SPA01, 2, -5)), silent = T)
}}else{
SPA01 <- "ID"
SPA02 <- sub("^wd:", "", IDs)
}}else{
if(any(!is.na(SPA00))){
SPA01 <- SPA00[!is.na(as.character(SPA00))]
SPA01 <- SPA01[grepl("@en", SPA01)]
try(SPA02 <- as.character(stringr::str_sub(SPA01, 2, -5)), silent = T)
}else{
SPA01 <- "ID"
SPA02 <- sub("^wd:", "", IDs)
}}

if(!exists("SPA")){return(message("Perhaps No Internet Services"))}

if(Message){message(paste(" Results" , "\n Data number:  ", nrow(SPA), sep=""))}

if(any(colnames(SPA) == "s0")){
SPA$s0 <- IDs
if(SPA01[1] != "ID"){
SPA$s0Label <- as.character(SPA01[1])
SPA$s.Lang <- stringr::str_sub(SPA[,2], -2, -1)
SPA$o.Lang <- stringr::str_sub(SPA[,4], -2, -1)
try(SPA[,2] <- stringr::str_sub(SPA[,2], 2, -5), silent = T)
try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
}else{
SPA$s0Label <- as.character(SPA02)
SPA$s.Lang <- "ID"
SPA$o.Lang <- stringr::str_sub(SPA[,4], -2, -1)
try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
}}else{
SPA$s.Lang <- stringr::str_sub(SPA[,2], -2, -1)
SPA$o.Lang <- stringr::str_sub(SPA[,4], -2, -1)
try(SPA[,2] <- stringr::str_sub(SPA[,2], 2, -5), silent = T)
try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
}

if(PREFIX_uri){
for(n in 1:dim(agGraphSearch::URI2Prefix)[1]){
#n <- 1
try(SPA[,1] <- gsub(paste0("^", agGraphSearch::URI2Prefix[n,2]), agGraphSearch::URI2Prefix[n,1], SPA[,1]), silent = T)
try(SPA[,1] <- gsub(paste0(agGraphSearch::URI2Prefix[n,3], "$"), "", SPA[,1]), silent = T)
try(SPA[,3] <- gsub(paste0("^", agGraphSearch::URI2Prefix[n,2]), agGraphSearch::URI2Prefix[n,1], SPA[,3]), silent = T)
try(SPA[,3] <- gsub(paste0(agGraphSearch::URI2Prefix[n,3], "$"), "", SPA[,3]), silent = T)
}
}

SPA$property <- p00
SPA$Depth <- PropertyPathDepth
SPA$triples <- paste0(SPA[,1], ".", p00, ".", SPA[,3])
colnames(SPA)[1:4] <- COL

if(JA){
#s
SPA <- SPA[order(SPA$s.Lang, decreasing = T),]
rownames(SPA) <- 1:nrow(SPA)
#o
SPA <- SPA[order(SPA$o.Lang, decreasing = T),]
rownames(SPA) <- 1:nrow(SPA)
}else{
#s
SPA <- SPA[order(SPA$s.Lang, decreasing = F),]
rownames(SPA) <- 1:nrow(SPA)
#o
SPA <- SPA[order(SPA$o.Lang, decreasing = F),]
rownames(SPA) <- 1:nrow(SPA)
}

#remove the duplicates
SPA <- SPA[rownames(unique(SPA["triples"])),]
rownames(SPA) <- 1:nrow(SPA)

return(data.frame(ID=IDs, Label=SPA02[1], SPA, stringsAsFactors = F))

}

############################################################
############################################################

CkeckQuery_PropertyPath <- function(Entity_ID,
                                    PropertyPathDepth,
                                    Upper=TRUE,
                                    p1,
                                    p2,
                                    FROM,
                                    EndPoint){

IDs <- Entity_ID
Prefix <- agGraphSearch::PREFIX

Query1 <-paste0('SELECT distinct ?oj ?oe ', '
', FROM, '
', 'WHERE {
optional{ ', IDs, ' rdfs:label ?oj . filter(LANG(?oj) = "ja") }
optional{ ', IDs, ' rdfs:label ?oe . filter(LANG(?oe) = "en") }
}')

############################################################
# Create query
############################################################
if(Upper){
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("IDs,  p1,  '?s1', '.
', 'optional{?s1 rdfs:label ?s1Label.} '
"))
}else{
c <- c(c, paste0("IDs,  p1,  '?s1', '.', '
', "))

for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.', '
', "))
}else{
c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.
', 'optional{?s", n-1," rdfs:label ?s", n-1, "Label.}
 optional{?s", n," rdfs:label ?s", n, "Label.} ' "))
}}}
c <- c(c, paste0("), ncol = 5, byrow = T)"))
eval(parse(text=paste(c, collapse = "")))

COL <- c("subject", "subjectLabel",
         "parentClass", "parentClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[3], paste(List00[3], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}
}else{
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("'?s1',  p1,   IDs , '.', '
optional{?s1 rdfs:label ?s1Label.} '
"))
}else{
c <- c(c, paste0("'?s1',  p2,   IDs , '.', '
', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', '
', "))
}else{
c <- c(c, paste0("'?s", n, "',  p1 ,'?s", n-1, "', '.',
'optional{?s", n-1," rdfs:label ?s", n-1, "Label.}
optional{?s", n," rdfs:label ?s", n, "Label.} '
                   "))
}}}
c <- c(c, paste0("), ncol = 5, byrow = T)"))
eval(parse(text=paste(c, collapse = "")))

COL <- c("subject", "subjectLabel",
         "childClass", "childClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[1], paste(List00[1], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}}
############################################################
############################################################
Query2 <-paste0('SELECT distinct ', PropertyPath01, '
', FROM, '
', 'WHERE {
',  QueryPath, '
}')


#create Query
Query <-paste0("EndPoint:
", EndPoint,"
Prefix: ",
Prefix,
'```````````````````````````````````````````',
'
', Query1, '
```````````````````````````````````````````',
'
', Query2, '
```````````````````````````````````````````')

#message(Query)
return( message(Query) )


}

