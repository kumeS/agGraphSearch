##' @title Merge ChEBI Labels
##'
##' @param Data a RDF data.
##' @param Labels a RDF data.
##'
##' @description This function is ...
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export MergeChEBIData
##'
##'

MergeChEBIData <- function(Data,
                          Labels){

#Parameters
Dat <- Data
ChEBILabels01 <- Labels[,c("Subject", "Object")]
#head(Dat); head(ChEBILabels01)

#head(Dat); dim(Dat)
colnames(Dat) <- tolower(colnames(Dat))
Dat$triple <- paste0(Dat$subject, ".", Dat$property, ".", Dat$parentclass)
rownames(Dat) <- 1:nrow(Dat)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
Dat <- Dat[,-ncol(Dat)]
#head(Dat); head(ChEBILabels01)

if(ncol(Dat) == 3){
colnames(Dat) <- c("subject", "property", "parentClass")
}

Dat <- Dat[order(Dat$subject, decreasing = T),]
if(all(colnames(ChEBILabels01) == c("Subject", "Object"))){
    Dat00 <-  merge(Dat, ChEBILabels01, by.x = "subject", by.y = "Subject", all = F, sort = F)
    Dat01 <-  merge(Dat00, ChEBILabels01, by.x = "parentClass", by.y = "Subject", all = F, sort = F)
    colnames(Dat01)[c((ncol(Dat01)-1):ncol(Dat01))] <- c("subjectLabel", "parentClassLabel")
    Dat02 <- Dat01[,c("subject", "property",
                  "parentClass",
                  "subjectLabel", "parentClassLabel")]
}
#head(Dat02)
return(Dat02)

}


