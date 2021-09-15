##' @title agJsTreeR: jsTree for agGraphSearch (modified)
##'
##' @param Data a input data.frame.
##' @param TopClass a string of the top class. ex. entity.wd:Q35120.
##' @param Brouse a logical
##' @param SaveHtmlFile a logical
##' @param FileName a character of file name
##' @param Output a character of folder directory
##' @param ExcludeQID a logical
##'
##' @description this function is to use jsTree for agGraphSearch data.
##'
##' @return html
##' @author Satoshi Kume
##' @export agJsTreeR
##' @importFrom jsTree jsTree
##' @importFrom igraph graph_from_edgelist
##' @importFrom igraph as_ids
##' @importFrom igraph shortest_paths
##' @importFrom filesstrings file.move
##' @importFrom htmlwidgets saveWidget
##'
##'

#library(jsTree)
agJsTreeR <- function(Data,
                     TopClass,
                     Brouse=TRUE,
                     SaveHtmlFile=TRUE,
                     FileName=NULL,
                     Output=NULL,
                     ExcludeQID=TRUE){

#Parameters
Width="125%"
Height="175%"

Dat <- Data
Dat$X <- paste0(Dat$subject)
Dat$Y <- paste0(Dat$parentClass)
DatXY <- Dat[,c("X", "Y")]
head(DatXY)

DatDF <- c()
BottomClass <- unique(DatXY$X)[!(unique(DatXY$X) %in% unique(DatXY$Y))]

#Obtain shortest path
for(m in 1:length(TopClass)){
#m <- 1
Datt <- c(); x <- 1; Query <- TopClass[m]
repeat{
b2 <- DatXY[DatXY$Y %in% Query,]
if(dim(b2)[1] == 0){break}
Datt <- rbind(Datt, b2)
Query <- b2$X
#message(x)
x <- x + 1
if(x == 50){break}
}

#Create graph data
graphList <- data.frame(from=Datt$X, to=Datt$Y, stringsAsFactors = F)
cc <- unique(c(Datt$X, Datt$Y)); cc0 <- cc[cc %in% BottomClass]
D00 <- igraph::graph_from_edgelist(as.matrix(graphList), directed = T)

for(n in seq_len(length(cc0))){
#n <- 1
a <- igraph::as_ids(igraph::shortest_paths(D00,
                       from = cc0[n],
                       to = TopClass[m])$vpath[[1]])
if(!identical(a, character(0))){
if(ExcludeQID){
if(!grepl("^wd:Q", a[1])){
b <- paste(a[length(a):1], collapse='/')
DatDF <- c(DatDF, b)
}
}else{
b <- paste(a[length(a):1], collapse='/')
DatDF <- c(DatDF, b)
}}}}

DatDF <- unique(DatDF)
if(any(DatDF == "NA")){DatDF <- DatDF[DatDF != "NA"]}
#DatDF
#head(Dat)
a1 <- paste0(Dat$subjectLabel, ".", Dat$subject)
a2 <- paste0(Dat$parentClassLabel, ".", Dat$parentClass)
a3 <- data.frame(X=c(Dat$subject, Dat$parentClass),
                 Y=c(a1, a2), row.names = 1:(length(a1)+length(a2)))
a4 <- a3[rownames(unique(a3["X"])),]
for(n in 1:nrow(a4)){
  DatDF <- gsub(a4[n,1], a4[n,2], DatDF)
}

if(Brouse){
jsTree::jsTree(DatDF, width=Width, height=Height)
}

if(is.null(FileName)){
if(SaveHtmlFile){
jsTree::jsTree(DatDF, width=Width, height=Height) %>%
  htmlwidgets::saveWidget("agJsTreeR.html")
if(!is.null(Output)){
suppressMessages(filesstrings::file.move(files="agJsTreeR.html", destinations=Output, overwrite = TRUE))
}
}
}else{
if(SaveHtmlFile){
jsTree::jsTree(DatDF, width=Width, height=Height) %>%
  htmlwidgets::saveWidget(FileName)
if(!is.null(Output)){
suppressMessages(filesstrings::file.move(files=FileName, destinations=Output, overwrite = TRUE))
}
}
}
}



