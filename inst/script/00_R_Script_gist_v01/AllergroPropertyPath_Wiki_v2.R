AllergroPropertyPath_Graph_up_v2 <- function(ID_Name="wd:Q268592", Depth=30){
Res <- c()
x <- 1
repeat{
print(x)
Res00 <- AllergroPropertyPath_Wiki_v2(ID_Name=ID_Name,
                                      PropertyPathDepth=x, Upper=T,
                                      Message=F,
                                      p1="wdt:P279|wdt:P31",
                                      p2="wdt:P279",
                                      JA=T,
                                      FROM="FROM <http://wikidata_nearly_full_201127>",
                                      EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj")  
if(is.null(Res00)){break}
Res[[x]] <- Res00
if(x >= Depth){
  break
}else{
  x <- x + 1
}
}

#length(Res)
#NULLなら終了
if(is.null(Res)){return(NULL)}

Res01 <- c()
for(n in 1:length(Res)){
Res01 <- Res01 %>% rbind(Res[[n]])  
}
#head(Res01)
rownames(Res01) <- 1:nrow(Res01)
#s
Res01 <- Res01[order(Res01$s.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)
#o
Res01 <- Res01[order(Res01$o.Lang, decreasing = T),]
rownames(Res01) <- 1:nrow(Res01)

#remove the duplicates
Res02 <- Res01[rownames(unique(Res01["unique"])),]
rownames(Res02) <- 1:nrow(Res02)

return(Res02)
}

AllergroPropertyPath_Wiki_v2 <- function(ID_Name="wd:Q2374463", 
                                         PropertyPathDepth=3, Upper=F,
                                         p1="wdt:P279|wdt:P31",
                                         p2="wdt:P279",
                                         Message=F,
                                         JA=T,
                                         FROM="FROM <http://wikidata_nearly_full_201127> ",
                                         EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"){

LABEL <- ID_Name
if(Message){message(paste(" QID:  ", LABEL, "\n", 
                            "Depth: ", PropertyPathDepth), sep="")}

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

Query <-paste0('SELECT distinct ?oj ?oe ',
               FROM, ' ', 
               'WHERE { 
               optional{ ', LABEL, ' rdfs:label ?oj . filter(LANG(?oj) = "ja") } 
               optional{ ', LABEL, ' rdfs:label ?oe . filter(LANG(?oe) = "en") } }')

suppressMessages(try(SPA00 <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))

if(Upper){
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("LABEL,  p1,  '?s1', '.', 'optional{?s1 rdfs:label ?s1Label.} ' "))  
}else{
c <- c(c, paste0("LABEL,  p1,  '?s1', '.', ' ', "))
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
c <- c(c, paste0("'?s1',  p1,   LABEL , '.', 'optional{?s1 rdfs:label ?s1Label.} ' "))
}else{
c <- c(c, paste0("'?s1',  p1,   LABEL , '.', ' ', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
  c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', ' ', "))
}else{
  c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', 
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

Query <-paste0('SELECT distinct ', PropertyPath01, ' ', 
               FROM, ' ', 
               'WHERE { ',  QueryPath, ' }' )

if(Message){message(paste("Query: ", LABEL, sep=""))}else{message(paste("", sep=""))}
suppressMessages(try(SPA <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))
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
SPA02 <- sub("^wd:", "", LABEL)
}}else{
if(any(!is.na(SPA00))){
SPA01 <- SPA00[!is.na(as.character(SPA00))]
SPA01 <- SPA01[grepl("@en", SPA01)]
try(SPA02 <- as.character(stringr::str_sub(SPA01, 2, -5)), silent = T)  
}else{
SPA01 <- "ID"
SPA02 <- sub("^wd:", "", LABEL)
}}

if(!exists("SPA")){return(message("Perhaps No Internet Services"))}

if(Message){message(paste(" Results" , "\n Data number:  ", nrow(SPA), sep=""))}
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,1] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,1]) %>% gsub("<", "",.) %>% gsub(">", "",.), silent = T)
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,2] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,2]) %>% gsub("<", "",.) %>% gsub(">", "",.), silent = T)

if(any(colnames(SPA) == "s0")){
SPA$s0 <- LABEL
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

SPA$property <- p00
SPA$Depth <- PropertyPathDepth
SPA$unique <- paste0(SPA[,1], ".", p00, ".", SPA[,3])
colnames(SPA)[1:4] <- COL

#s
SPA <- SPA[order(SPA$s.Lang, decreasing = T),]
rownames(SPA) <- 1:nrow(SPA)

#o
SPA <- SPA[order(SPA$o.Lang, decreasing = T),]
rownames(SPA) <- 1:nrow(SPA)

#remove the duplicates
SPA <- SPA[rownames(unique(SPA["unique"])),]
rownames(SPA) <- 1:nrow(SPA)

#ラベルがない部分の補完
#SPA$o.Lang[grepl("^Q[1-9][0-9]", SPA[,4])] <- "ID"
SPA$o.Lang[grepl("^Q[1-9][0-9][0-9]", SPA[,4])] <- "ID"
SPA$s.Lang[grepl("^Q[1-9][0-9][0-9]", SPA[,2])] <- "ID"

#ラベルNAの場合
#head(SPA)
SPA[,5][is.na(SPA[,2])] <- "ID"
SPA[,2][is.na(SPA[,2])] <- sub("^wd:", "", SPA[,1][is.na(SPA[,2])])

SPA[,6][is.na(SPA[,4])] <- "ID"
SPA[,4][is.na(SPA[,4])] <- sub("^wd:", "", SPA[,3][is.na(SPA[,4])])

#SPA

return(data.frame(QID=LABEL, LabeL=SPA02[1], SPA, stringsAsFactors = F))

}
