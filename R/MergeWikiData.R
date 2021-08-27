##' @title Merge Labels
##'
##' @param Data a RDF data.
##' @param Labels a RDF data.
##'
##' @description This function is ...
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export MergeWikiData
##'
##'

MergeWikiData <- function(Data,
                          Labels){
#Parameters
Dat <- Data
Labels01 <- Labels
#head(Labels01)
#table(Labels01$Property)
Labels01 <- Labels01[Labels01$Property == "rdfs:label",]
Labels1 <- Labels01[Labels01[,ncol(Labels01)] == "@ja",]
Labels2 <- Labels01[Labels01[,ncol(Labels01)] == "@en",]

#bind
Labels02 <- rbind(Labels1, Labels2)
rownames(Labels02) <- 1:nrow(Labels02)
Labels02 <- Labels02[as.numeric(rownames(unique(Labels02["Subject"]))),]
Labels02 <- Labels02[,c("Subject", "Object")]
#head(Labels02)
#dim(Labels02)

if(!grepl("^wd", Labels02$Subject[1])){
cc <- grepl("^wd", Labels02$Subject)
Labels02$Subject[cc] <- paste0("wd:", Labels02$Subject[cc])
}

#head(Dat); dim(Dat)
#head(Labels02); dim(Labels02)
Dat$triple <- paste0(Dat$subject, ".", Dat$property, ".", Dat$parentClass)
rownames(Dat) <- 1:nrow(Dat)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
Dat <- Dat[,-ncol(Dat)]

if(ncol(Dat) == 3){
  colnames(Dat) <- c("subject", "property", "parentClass")
}

Dat <- Dat[order(Dat$subject, decreasing = T),]
#head(Dat)
if(all(colnames(Labels02) == c("Subject", "Object"))){
    Dat00 <-  merge(Dat, Labels02, by.x = "subject", by.y = "Subject", all = F,
                    all.x = T, all.y = F, sort = F)
    Dat01 <-  merge(Dat00, Labels02, by.x = "parentClass", by.y = "Subject", all = F,
                    all.x = T, all.y = F, sort = F)
    colnames(Dat01)[c((ncol(Dat01)-1):ncol(Dat01))] <- c("subjectLabel", "parentClassLabel")
    Dat02 <- Dat01[,c("subject", "property", "parentClass",
                  "subjectLabel", "parentClassLabel")]
    Dat02$subjectLabel[is.na(Dat02$subjectLabel)] <- Dat02$subject[is.na(Dat02$subjectLabel)]
    Dat02$parentClassLabel[is.na(Dat02$parentClassLabel)] <- Dat02$parentClass[is.na(Dat02$parentClassLabel)]
}

#head(Dat02); dim(Dat02)
return(Dat02)

}


