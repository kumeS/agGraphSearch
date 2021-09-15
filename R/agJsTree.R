##' @title agJsTree: jsTree for agGraphSearch
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
##' @export agJsTree
##' @importFrom jsTree jsTree
##' @importFrom igraph graph_from_edgelist
##' @importFrom igraph as_ids
##' @importFrom igraph shortest_paths
##' @importFrom filesstrings file.move
##'
##'

#library(jsTree)
agJsTree <- function(Data,
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

#Create graph data
graphList <- data.frame(from=DatXY$X, to=DatXY$Y, stringsAsFactors = F)
D00 <- igraph::graph_from_edgelist(as.matrix(graphList), directed = T)

#Obtain shortest path
for(n in seq_len(length(BottomClass))){
#n <- 1
a <- igraph::as_ids(igraph::shortest_paths(D00,
                       from = BottomClass[n],
                       to = TopClass)$vpath[[1]])

if(ExcludeQID){
if(!grepl("^wd:Q", a[1])){
b <- paste(a[length(a):1], collapse='/')
DatDF <- c(DatDF, b)
}
}else{
b <- paste(a[length(a):1], collapse='/')
DatDF <- c(DatDF, b)
}
}

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
  htmlwidgets::saveWidget("agJsTree.html")
if(!is.null(Output)){
suppressMessages(filesstrings::file.move(files="agJsTree.html", destinations=Output, overwrite = TRUE))
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



