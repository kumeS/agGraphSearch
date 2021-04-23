if(!require("SPARQL")){install.packages("SPARQL")}; library(SPARQL)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("stringr")){install.packages("stringr")}; library(stringr)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("furrr")){install.packages("furrr")}; library(furrr)
if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("VennDiagram")){install.packages("VennDiagram")}; library(VennDiagram)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("plotly")){install.packages("plotly")}; library(plotly)

##メモリ割当設定
#memory.limit(size = 24000)

############################################################################################
############################################################################################
AllergroPropertyPath_Graph_up_v2 <- function(ID_Name="wd:Q268592", 
                                             Depth=30, 
                                             Message=F){
Res <- c()
x <- 1
repeat{
if(Message){ print(x) }
try(Res00 <- AllergroPropertyPath_v2(ID_Name=ID_Name,
                                      PropertyPathDepth=x, Upper=T,
                                      Message=F,
                                      p1="wdt:P279|wdt:P31",
                                      p2="wdt:P279",
                                      JA=T,
                                      FROM="FROM <http://wikidata_nearly_full_201127> ",
                                      EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj") , silent=T)
if(is.null(Res00)){break}
Res[[x]] <- Res00
if(x >= Depth){
  break
}else{
  x <- x + 1
}}

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

#order depth
Res02 <- Res02[order(Res02$Depth, decreasing = F),]
rownames(Res02) <- 1:nrow(Res02)

return(Res02)

}

############################################################################
AllergroPropertyPath_NEDOpj_up_v1 <- function(ID_Name="wd:Q268592", Depth=30, Message=F){
Res <- c()
x <- 1
repeat{
if(Message){ print(x) }
Res00 <- AllergroPropertyPath_v2(ID_Name=ID_Name,
                                      PropertyPathDepth=x, Upper=T,
                                      Message=F,
                                      p1="rdfs:subClassOf",
                                      p2="rdfs:subClassOf",
                                      JA=T,
                                      FROM="FROM <http://nedo_jp> FROM <http://cw_2021_1_29_N> ",
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

######################################################################
######################################################################
AllergroPropertyPath_Graph_down_v2 <- function(ID_Name="wd:Q81163", Depth=2, Message=T){
Res <- c()
x <- 1
repeat{
if(Message){ print(x) }
if(x == Depth){
  p1 <- "wdt:P279|wdt:P31"
  p2 <- "wdt:P279"
}else{
  p1 <- "wdt:P279"
  p2 <- "wdt:P279"
}

Res00 <- AllergroPropertyPath_v2(ID_Name=ID_Name,
                                      PropertyPathDepth=x, Upper=F,
                                      Message=F,
                                      p1=p1,
                                      p2=p2,
                                      JA=T,
                                      FROM="FROM <http://wikidata_nearly_full_201127> ",
                                      EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj")  
if(is.null(Res00)){break}
Res[[x]] <- Res00  
if(x >= Depth){
  break
}else{
  x <- x + 1
}
}

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
#head(Res02)
return(Res02)
}

######################################################################
AllergroPropertyPath_Graph_down_v3 <- function(ID_Name="wd:Q81163", 
                                               Depth=2, 
                                               Message=T,
                                               Prop=T){
Res <- c()
x <- 1

repeat{
if(Message){ print(x) }
if(Prop){
  p1 <- "wdt:P279|wdt:P31"
  p2 <- "wdt:P279|wdt:P31"
}else{
  p1 <- "wdt:P279"
  p2 <- "wdt:P279"
}

Res00 <- AllergroPropertyPath_v2(ID_Name=ID_Name,
                                      PropertyPathDepth=x, Upper=F,
                                      Message=F,
                                      p1=p1,
                                      p2=p2,
                                      JA=T,
                                      FROM="FROM <http://wikidata_nearly_full_201127> ",
                                      EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj")  
if(is.null(Res00)){break}
Res[[x]] <- Res00  
if(x >= Depth){
  break
}else{
  x <- x + 1
}
}

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
#head(Res02)
return(Res02)
}

##########################################################################################
##########################################################################################
AllergroPropertyPath_Graph_up_down_v2 <- function(ID_Name=SearchLabel$CommonEntity[78], Hierarchy=SearchLabel$Levels[78]){
#ID_Name=SearchLabel$CommonEntity[1]; Hierarchy=SearchLabel$Levels[1]

#上位
print("up")
try(Graph_up <- AllergroPropertyPath_Graph_up_v2(ID_Name=ID_Name, Depth=30), silent=T)

#下位
print("down")
try(Graph_down <- AllergroPropertyPath_Graph_down_v2(ID_Name=ID_Name, Depth=Hierarchy), silent=T)

#head(Graph_up)
#head(Graph_down)
#colnames(Graph_up); colnames(Graph_down)
#colnames(Graph_up) == colnames(Graph_down)
Graph_up <- Graph_up[,-c(1:2)]
Graph_down <- Graph_down[,-c(1:2)]

if(is.null(Graph_up) == TRUE && is.null(Graph_down) == TRUE){return(NULL)}

if(is.null(Graph_up) == FALSE && is.null(Graph_down) == FALSE){
#colnames(Graph_up); colnames(Graph_down)
#head(Graph_down)
#head(Graph_down_r)
Graph_down_r <- Graph_down[,c(3:4, 1:2, 6:5, 7:9)]
colnames(Graph_down_r) <- colnames(Graph_up)

Graph_up$UpDown <- "up"
Graph_down_r$UpDown <- "down"
GraphDat <- rbind(Graph_up, Graph_down_r)
}else{
  if(is.null(Graph_up) == FALSE && is.null(Graph_down) == TRUE){
    Graph_up$UpDown <- "up"
    GraphDat <- Graph_up
  }else{
    Graph_down_r <- Graph_down[,c(3:4, 1:2, 6:5, 7:9)]
    colnames(Graph_down_r) <- c("subject","subjectLabel", "parentClass","parentClassLabel",
                                "s.Lang", "o.Lang", "property", "Depth", "unique")
    Graph_down_r$UpDown <- "down"
    GraphDat <- Graph_down_r
}}
return(GraphDat)
}


##########################################################################################
##########################################################################################
Graph_UpDown <- function(Label=SearchLabel$CommonEntity[1], Hierarchy=SearchLabel$Levels[1]){

#Label=SearchLabel$CommonEntity[1]; Hierarchy=SearchLabel$Levels[1]
#上位
print("up")
Graph_up <- wikiGraph_plusWD_v2(WD_EntityID=Label, Depth=99, Upper=T)
#table(unlist(future_map(wikiGraph_01, nrow, .progress = TRUE)))
#SPAresults

#下位
print("down")
Graph_down <- c()
for(n in seq_len(Hierarchy)){
#n <- 1
Graph_down00 <- wikiPropertyPath_v1(QID_Name=Label, PropertyPathDepth=n, lang=1, Upper=F)
Graph_down00$group <- Graph_down00$subject
Graph_down00$Depth <- n
Graph_down00$Unique <- paste(Graph_down00$subject, ".", Graph_down00$childClass, sep="")
Graph_down <- Graph_down %>% rbind(Graph_down00)
}

#head(Graph_up)
#head(Graph_down)
#colnames(Graph_up); colnames(Graph_down)
#colnames(Graph_up) == colnames(Graph_down)
Graph_up <- Graph_up[,-c(3:4)]
Graph_down <- Graph_down[,-c(1:2,7)]

if(is.null(Graph_up) == TRUE && is.null(Graph_down) == TRUE){
  return(NULL)
}else{}

if(is.null(Graph_up) == FALSE && is.null(Graph_down) == FALSE){
#head(Graph_down)
#head(Graph_down_r)
Graph_down_r <- Graph_down[,c(3:4,1:2,5:7)]
colnames(Graph_down_r) <- colnames(Graph_up)

Graph_up$UpDown <- "up"
Graph_down_r$UpDown <- "down"
  
GraphDat <- rbind(Graph_up, Graph_down_r)
  
}else{
  if(is.null(Graph_up) == FALSE && is.null(Graph_down) == TRUE){
    Graph_up$UpDown <- "up"
    GraphDat <- Graph_up
  }else{
    Graph_down_r <- Graph_down[,c(3:4,1:2,5:7)]
    colnames(Graph_down_r) <- c("subject","subjectLabel",
                                "parentClass","parentClassLabel",
                                "group", "Depth", "Unique")
    Graph_down_r$UpDown <- "down"
    GraphDat <- Graph_down_r
}}
return(GraphDat)
}


######################################################################
######################################################################
#両ラベルある
#ID_Name="wd:Q21198"; PropertyPathDepth=1; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q21198"; PropertyPathDepth=1; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

#ID_Name="wd:Q21198"; PropertyPathDepth=4; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q21198"; PropertyPathDepth=4; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q21198"; PropertyPathDepth=3; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

#英語のみ
#ID_Name="wd:Q3856198"; PropertyPathDepth=1; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q19868531"; PropertyPathDepth=1; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q19868531"; PropertyPathDepth=2; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

#両方なし
#ID_Name="wd:Q18139218"; PropertyPathDepth=1; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q18139218"; PropertyPathDepth=2; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#ID_Name="wd:Q60982855"; PropertyPathDepth=3; Upper=T; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

#実例
#ID_Name="wd:Q81163";PropertyPathDepth=16; Upper=T;Message=F;p1="rdfs:subClassOf";p2="rdfs:subClassOf";JA=T;FROM="FROM <http://nedo_jp> FROM <http://cw_2021_1_29_N> ";EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"
#AllergroPropertyPath_v2(ID_Name="wd:Q2374463", PropertyPathDepth=3)

#ID_Name=SearchLabel$CommonEntity[78]; PropertyPathDepth=1; Upper=F; JA=T; p1="wdt:P279|wdt:P31"; p2="wdt:P279"; Message=F; FROM="FROM <http://wikidata_nearly_full_201127>"; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

AllergroPropertyPath_v2 <- function(ID_Name="wd:Q2374463", 
                                    PropertyPathDepth=2, 
                                    Upper=F, 
                                    p1="wdt:P279|wdt:P31", 
                                    p2="wdt:P279", 
                                    Message=F, 
                                    JA=T, 
                                    FROM="FROM <http://wikidata_nearly_full_201127> ", 
                                    EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj", 
                                    Path="./05_Out"){

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

Query <-paste0('SELECT distinct ?oj ?oe ', ' ',
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
c <- c(c, paste0("'?s1',  p2,   LABEL , '.', ' ', "))
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

Query <-paste0('SELECT distinct ', PropertyPath01, ' ', 
               FROM, ' ', 
               'WHERE { ',  QueryPath, ' }' )

if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(A <- try(SPA <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))
if(class(A) == "try-error"){
    DD <- data.frame(ID_Name=ID_Name,
                     PropertyPathDepth=PropertyPathDepth, 
                     Upper=Upper,
                     p1=p1,
                     p2=p2,
                     Message=Message,
                     JA=JA,
                     FROM=FROM,
                     EndPoint=EndPoint,
                     Path=Path)
    readr::write_excel_csv(data.frame(DD), append=T, col_names=F,
                           file= paste0(Path, "/", "try-error.csv"))
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

return(data.frame(QID=LABEL, Label=SPA02[1], SPA, stringsAsFactors = F))

}

################################################################################################


################################################################################################
################################################################################################
#ID_Name="wd:Q2374463"; PropertyPathDepth=1;  lang=1; Upper=F; p1="rdfs:subClassOf"; p2="rdfs:subClassOf"; Message=F; EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj"

AllergroPropertyPath_v1 <- function(ID_Name="wd:Q2374463", 
                                    PropertyPathDepth=1, Upper=T,
                                    p1="rdfs:subClassOf",
                                    p2="rdfs:subClassOf",
                                    Message=F,
                                    FROM = "" ,
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
'

Query <-paste('SELECT distinct ?o', ' ',
               FROM, ' ',
               'WHERE { ', LABEL, ' rdfs:label ?o . }', sep="")

suppressMessages(try(SPA00 <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))
SPA01 <- as.character(SPA00)[!is.na(as.character(SPA00))]
try(SPA01 <- stringr::str_sub(SPA01, 2, -5), silent = T)

if(Upper){
c <- paste0("List <- matrix(c(")
if(PropertyPathDepth == 1){
c <- c(c, paste0("LABEL,  p1,  '?s1', '.', '?s1 rdfs:label ?s1Label.' "))
}else{
c <- c(c, paste0("LABEL,  p1,  '?s1', '.', '?s1 rdfs:label ?s1Label.', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
  c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.', '?s", n," rdfs:label ?s", n, "Label.', "))
}else{
  c <- c(c, paste0("'?s", n-1, "',  p2 ,'?s", n, "', '.', '?s", n," rdfs:label ?s", n, "Label.' "))
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
c <- c(c, paste0("'?s1',  p1,   LABEL , '.', '?s1 rdfs:label ?s1Label.' "))
  }else{
c <- c(c, paste0("'?s1',  p1,   LABEL , '.', '?s1 rdfs:label ?s1Label.', "))
for(n in 2:PropertyPathDepth){
# n <- 2
if(n != PropertyPathDepth){
  c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', '?s", n," rdfs:label ?s", n, "Label.', "))
}else{
  c <- c(c, paste0("'?s", n, "',  p2 ,'?s", n-1, "', '.', '?s", n," rdfs:label ?s", n, "Label.' "))
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

Query <-paste('SELECT distinct ', PropertyPath01, ' ',
              FROM, ' ', 
              'WHERE { ',  QueryPath, ' }', 
              sep="")
  
if(Message){message(paste("Query: ", LABEL, sep=""))}
suppressMessages(try(SPA <- data.frame(SPARQL(url=EndPoint, query=paste(Prefix, Query))$results), silent = T))

if(nrow(SPA) == 0){return(NULL)}

if(!exists("SPA")){return(message("Perhaps No Internet Services"))}

if(Message){message(paste(" Results" , "\n Data number:  ", nrow(SPA), sep=""))}
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,1] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,1]) %>% gsub("<", "",.) %>% gsub(">", "",.), silent = T)
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,2] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,2]) %>% gsub("<", "",.) %>% gsub(">", "",.), silent = T)

if(any(colnames(SPA) == "s0")){
SPA$s0 <- LABEL
SPA$s0Label <- SPA01[1]
try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
}else{
try(SPA[,2] <- stringr::str_sub(SPA[,2], 2, -5), silent = T)
try(SPA[,4] <- stringr::str_sub(SPA[,4], 2, -5), silent = T)
}

colnames(SPA) <- COL
return(data.frame(QID=LABEL, Label=SPA01[1], SPA, p=p00))

}

################################################################################################
################################################################################################
################################################################################################
AllergroPropertyPath_UpDown_v1 <- function(ID="wd:Q81163", LowHierarchy=1){

#上位
print("up")
Graph_up <- c()
for(m in seq_len(10)){
#m <- 10
Graph_up00 <- AllergroPropertyPath_v1(ID_Name=ID, PropertyPathDepth=m, Upper=T)  
if(nrow(Graph_up00) > 0){
Graph_up00$group <- Graph_up00$subject
Graph_up00$Depth <- m
Graph_up00$Unique <- paste(Graph_up00$subject, ".", Graph_up00$parentClass, sep="")
Graph_up <- Graph_up %>% rbind(Graph_up00)
}}

#下位
print("down")
Graph_down <- c()
for(n in seq_len(LowHierarchy)){
#n <- 1
Graph_down00 <- AllergroPropertyPath_v1(ID_Name=ID, PropertyPathDepth=n, Upper=F)
Graph_down00$group <- Graph_down00$subject
Graph_down00$Depth <- n
Graph_down00$Unique <- paste(Graph_down00$subject, ".", Graph_down00$childClass, sep="")
Graph_down <- Graph_down %>% rbind(Graph_down00)
}



#head(Graph_up)
#head(Graph_down)
#colnames(Graph_up); colnames(Graph_down)
#colnames(Graph_up) == colnames(Graph_down)

if(is.null(Graph_up) == TRUE && is.null(Graph_down) == TRUE){
  return(NULL)
}else{}

if(is.null(Graph_up) == FALSE && is.null(Graph_down) == FALSE){
#head(Graph_down)
#head(Graph_down_r)
Graph_down_r <- Graph_down[,c(1:2, 5:6, 3:4,7:10)]
colnames(Graph_down_r) <- colnames(Graph_up)

Graph_up$UpDown <- "up"
Graph_down_r$UpDown <- "down"
  
GraphDat <- rbind(Graph_up, Graph_down_r)
  
}else{
  if(is.null(Graph_up) == FALSE && is.null(Graph_down) == TRUE){
    Graph_up$UpDown <- "up"
    GraphDat <- Graph_up
  }else{
    Graph_down_r <- Graph_down[,c(1:2, 5:6, 3:4,7:10)]
    colnames(Graph_down_r) <- c("QID","Label", 
                                "subject","subjectLabel",
                                "parentClass","parentClassLabel",
                                "p",  "group", "Depth", "Unique")
    Graph_down_r$UpDown <- "down"
    GraphDat <- Graph_down_r
  }}
rownames(GraphDat) <- 1:nrow(GraphDat)
GraphDat <- GraphDat[rownames(unique(GraphDat["Unique"])),]
return(GraphDat)
}


################################################################################################
################################################################################################
#Graph=b; NodeColorRandom=F; Count=2; Size=10; SmallSize=5; StarSize=10; FontSize=7; HeightSclale = "750px"; WidthSclale = "110%";
#SEED=123; Selected=NULL; Browse=FALSE; output=FALSE; file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""); outputNodesEdges=FALSE

AllergroPropertyPath_UpDown_VisNetwork <- function(Graph=GraphData, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                           SEED=123, Selected=NULL, Browse=FALSE, output=FALSE, file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE){

if(is.null(Graph)){return("NULL")}
set.seed(SEED)
#head(Graph)
Graph$subjectLabel <- paste(Graph$subjectLabel, ".", Graph$subject, sep="")
Graph$parentClassLabel <- paste(Graph$parentClassLabel, ".", Graph$parentClass, sep="")

#if(nrow(Graph) > 5000){message("Too huge data"); break()}
LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$parentClassLabel, Graph$parentClassLabel), stringsAsFactors = F)
Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel)

if(Count > 1){
    Count10 <- table(nodes$group)[order(table(nodes$group))]
    group2 <- nodes$group
    id2 <- nodes$id
    if(NodeColorRandom){
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    }else{
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))
    }
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        #n <- 241
        nodes$color.background[group2 == names(Count10)[n]] <- ColorSpa[n]
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
        nodes$size[group2 == names(Count10)[n]] <- Size
     }else{
        #nodes$size[group2 == names(Count10)[n]] <- SmallSize
        #nodes$group[group2 == names(Count10)[n]] <- "Others"
     }}
  }else{}
  
if(Count > 1){
      for(n in seq_len(length(Count10))){
        if(Count10[n] > Count){
          #n <- 241
          #head(nodes)
          nodes$color.background[id2 == names(Count10)[n]] <- ColorSpa[n]
          nodes$color.border[id2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
          nodes$size[id2 == names(Count10)[n]] <- Size
          nodes$group[id2 == names(Count10)[n]] <- names(Count10)[n]
        }else{}}
}else{}

nodes <- nodes[as.numeric(as.character(rownames(unique(nodes['id'])))),]
#head(nodes)
Lab00 <- strsplit(nodes$id, ".wd:")
Lab01 <- data.frame(matrix(NA, nrow=length(Lab00), ncol=2))
for(n in 1:length(Lab00)){Lab01[n,] <- Lab00[[n]]}
#head(Lab01); tail(Lab01)
#head(Lab02); tail(Lab02)
Lab01$id <- paste("wd:", Lab01$X2, sep="")

search <- data.frame(ID=searchClass(Class="http://class/search/"), search="search")
common <- data.frame(ID=searchClass(Class="http://class/common/"), common="common")
commonExpandDownward <- data.frame(ID=searchClass(Class="http://class/commonExpandDownward/"),
                                   commonExpandDownward="commonExpandDownward")

nodes$color.background[Lab01$id %in% search$ID] <- "skyblue"
nodes$size[Lab01$id %in% search$ID] <- Size*2
nodes$color.background[Lab01$id %in% common$ID] <- "lightgreen"
nodes$size[Lab01$id %in% common$ID] <- Size*2
nodes$color.background[Lab01$id %in% commonExpandDownward$ID] <- "yellow"
nodes$size[Lab01$id %in% commonExpandDownward$ID] <- Size*2

if(is.null(Selected)){
  VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(outputNodesEdges){
    readr::write_excel_csv(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="")) 
    readr::write_excel_csv(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="")) 
  }else{}

  if(output){
    VIS %>% saveNetwork(file = file)
    if(Browse){browseURL(file)}else{return(NULL)}
  } else {}

  return(VIS)
}


########################################################################
########################################################################



