##' @title Search class-related hierarchy in wikidata by the property path of SPARQL query.
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param Message logical; perform an output of EntityName or not.
##'
##' @export PropertyPath_GraphUp_Wikidata
##' @export PropertyPath_GraphUp_Wikidata_P279
##'
##'


PropertyPath_GraphUp_Wikidata <- function(Entity_ID,
                                          Depth=30,
                                          Message=FALSE){

#Parameters
Upper=TRUE
p1="wdt:P279|wdt:P31"
p2="wdt:P279"
JA=TRUE
EndPoint=agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM=agGraphSearch::KzLabEndPoint_Wikidata$FROM
FolferPath="03_Out"
PREFIX_uri=TRUE

Res <- c()
x <- 1

repeat{
if(Message){ print(x) }
try(Res00 <- agGraphSearch::PropertyPath(Entity_ID=Entity_ID,
                                         PropertyPathDepth=x,
                                         Upper=Upper,
                                         Message=Message,
                                         p1=p1,
                                         p2=p2,
                                         JA=JA,
                                         FROM=FROM,
                                         EndPoint=EndPoint,
                                         FolferPath=FolferPath,
                                         PREFIX_uri=PREFIX_uri), silent=T)
if(is.null(Res00)){break}
Res[[x]] <- Res00
if(x >= Depth){
  break
}else{
  x <- x + 1
}}

#head(Res[[1]])
#The run finish if Res is NULL
if(is.null(Res)){return(NULL)}

#Bind the results
Res01 <- ListDF2DF(Res)
rownames(Res01) <- 1:nrow(Res01)

if(JA){
#s
Res01 <- Res01[order(Res01$s.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)
#o
Res01 <- Res01[order(Res01$o.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)
}else{
#s
Res01 <- Res01[order(Res01$s.Lang, decreasing = F),]
rownames(Res01) <- 1:nrow(Res01)
#o
Res01 <- Res01[order(Res01$o.Lang, decreasing = F),]
rownames(Res01) <- 1:nrow(Res01)
}

#remove the duplicates
Res02 <- Res01[rownames(unique(Res01["triples"])),]
rownames(Res02) <- 1:nrow(Res02)

#order depth
Res02 <- Res02[order(Res02$Depth, decreasing = F),]
rownames(Res02) <- 1:nrow(Res02)

#colnames(Res02)
Res02$s.Lang[grepl("^Q[1-9]", Res02[,4])] <- "ID"
Res02$o.Lang[grepl("^Q[1-9]", Res02[,6])] <- "ID"

#label NA
Res02[,7][is.na(Res02[,4])] <- "ID"
Res02[,4][is.na(Res02[,4])] <- sub("^wd:", "", Res02[,3][is.na(Res02[,4])])
Res02[,8][is.na(Res02[,6])] <- "ID"
Res02[,6][is.na(Res02[,6])] <- sub("^wd:", "", Res02[,5][is.na(Res02[,6])])

return(Res02)

}


PropertyPath_GraphUp_Wikidata_P279 <- function(Entity_ID,
                                          Depth=30,
                                          Message=FALSE){

#Parameters
Upper=TRUE
p1="wdt:P279"
p2="wdt:P279"
JA=TRUE
EndPoint=agGraphSearch::KzLabEndPoint_Wikidata$EndPoint
FROM=agGraphSearch::KzLabEndPoint_Wikidata$FROM
FolferPath="03_Out"
PREFIX_uri=TRUE

Res <- c()
x <- 1

repeat{
if(Message){ print(x) }
try(Res00 <- agGraphSearch::PropertyPath(Entity_ID=Entity_ID,
                                         PropertyPathDepth=x,
                                         Upper=Upper,
                                         Message=Message,
                                         p1=p1,
                                         p2=p2,
                                         JA=JA,
                                         FROM=FROM,
                                         EndPoint=EndPoint,
                                         FolferPath=FolferPath,
                                         PREFIX_uri=PREFIX_uri), silent=T)
if(is.null(Res00)){break}
Res[[x]] <- Res00
if(x >= Depth){
  break
}else{
  x <- x + 1
}}

#head(Res[[1]])
#The run finish if Res is NULL
if(is.null(Res)){return(NULL)}

#Bind the results
Res01 <- ListDF2DF(Res)
rownames(Res01) <- 1:nrow(Res01)

if(JA){
#s
Res01 <- Res01[order(Res01$s.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)
#o
Res01 <- Res01[order(Res01$o.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)
}else{
#s
Res01 <- Res01[order(Res01$s.Lang, decreasing = F),]
rownames(Res01) <- 1:nrow(Res01)
#o
Res01 <- Res01[order(Res01$o.Lang, decreasing = F),]
rownames(Res01) <- 1:nrow(Res01)
}

#remove the duplicates
Res02 <- Res01[rownames(unique(Res01["triples"])),]
rownames(Res02) <- 1:nrow(Res02)

#order depth
Res02 <- Res02[order(Res02$Depth, decreasing = F),]
rownames(Res02) <- 1:nrow(Res02)

#colnames(Res02)
Res02$s.Lang[grepl("^Q[1-9]", Res02[,4])] <- "ID"
Res02$o.Lang[grepl("^Q[1-9]", Res02[,6])] <- "ID"

#label NA
Res02[,7][is.na(Res02[,4])] <- "ID"
Res02[,4][is.na(Res02[,4])] <- sub("^wd:", "", Res02[,3][is.na(Res02[,4])])
Res02[,8][is.na(Res02[,6])] <- "ID"
Res02[,6][is.na(Res02[,6])] <- sub("^wd:", "", Res02[,5][is.na(Res02[,6])])

return(Res02)

}



