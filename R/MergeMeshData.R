##' @title Merge Labels
##'
##' @param Data a RDF data.
##' @param Labels a RDF data.
##'
##' @description This function is ...
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export MergeMeshData
##'
##'

MergeMeshData <- function(Data,
                          Labels){
#Parameters
Dat <- Data
MeshLabels01 <- Labels[,c("Subject", "Object")]

#head(Dat); dim(Dat)
Dat$triple <- paste0(Dat$subject, ".", Dat$property, ".", Dat$parentClass)
rownames(Dat) <- 1:nrow(Dat)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
Dat <- Dat[,-ncol(Dat)]

if(ncol(Dat) == 4){
  colnames(Dat) <- c("subject", "property", "parentClass", "OtherInfo")
  }else{
  Dat <- Dat[,c("subject", "property", "parentClass", "OtherInfo")]
}

Dat <- Dat[order(Dat$subject, decreasing = T),]
if(all(colnames(MeshLabels01) == c("Subject", "Object"))){
    Dat00 <-  merge(Dat, MeshLabels01, by.x = "subject", by.y = "Subject", all = F, sort = F)
    Dat01 <-  merge(Dat00, MeshLabels01, by.x = "parentClass", by.y = "Subject", all = F, sort = F)
    colnames(Dat01)[c((ncol(Dat01)-1):ncol(Dat01))] <- c("subjectLabel", "parentClassLabel")
    Dat02 <- Dat01[,c("subject", "property",
                  "parentClass", "OtherInfo",
                  "subjectLabel", "parentClassLabel")]
}
#head(Dat02)
return(Dat02)

}


