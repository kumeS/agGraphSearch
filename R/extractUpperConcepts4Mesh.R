##' @title The upper class search of Mesh RDF
##'
##' @param Lab_List a character vector of input.
##' @param Data a RDF data.
##' @param Labels a RDF data.
##' @param Property a string vector of property.
##'
##' @description This function is a function to obtain
##' the upper class (meshv:broaderDescriptor) of Mesh RDF.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export extractUpperConcepts4Mesh
##'
##'

extractUpperConcepts4Mesh <- function(Lab_List,
                                      Data,
                                      Labels,
                                      Property){

#Parameters
List <- Lab_List
MeshOB <- Data
MeshLabels01 <- Labels
List <- List[List %in% MeshOB$Subject]

if(!is.vector(Lab_List)){ return(message("Warning: Not proper value of Lab_List")) }
if(!is.vector(Property)){ return(message("Warning: Not proper value of Property")) }

if(!all(colnames(MeshOB) %in% c("Subject","Property","Object","OtherInfo"))){
   MeshOB <- MeshOB[,colnames(MeshOB) %in% c("Subject","Property","Object","OtherInfo")]
}

if(!all(colnames(MeshOB) == c("Subject","Property","Object","OtherInfo"))){
   return(message("Warning: Not proper value of Data"))
}

MeshOB00 <- MeshOB[c(MeshOB$Property %in% Property),]

DatFinal <- c()
for(n in seq_len(length(List))){
#n <- 1
print(n)
x <- 1
List00 <- List[n]
Dat <- c()

repeat{
#print(x)
MeshOB01 <- MeshOB00[c(MeshOB00$Subject %in% List00),]
if(nrow(MeshOB01) != 0 && x < 50){
  Dat <- Dat %>% rbind(MeshOB01)
}else{
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Object)
  rownames(Dat) <- 1:nrow(Dat)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
  Dat <- Dat[,-ncol(Dat)]
  if(ncol(Dat) == 4){colnames(Dat) <- c("subject", "property", "parentClass", "OtherInfo")}
  Dat <- Dat[order(Dat$subject, decreasing = T),]
  if(any(colnames(MeshLabels01[,c("Subject", "Object")]) == c("Subject", "Object"))){
    Dat00 <-  merge(Dat, MeshLabels01, by.x = "subject", by.y = "Subject", all = F, sort = F)
    Dat01 <-  merge(Dat00, MeshLabels01, by.x = "parentClass", by.y = "Subject", all = F, sort = F)
    colnames(Dat01)[c((ncol(Dat01)-1):ncol(Dat01))] <- c("subjectLabel", "parentClassLabel")
    Dat02 <- Dat01[,c("subject", "property",
                  "parentClass", "subjectLabel", "parentClassLabel")]
  }
  Top <- Dat02$parentClass[!(unique(Dat02$parentClass) %in% unique(Dat02$subject))]
  DatFinal[[n]] <- list(Dat02, Top)
  names(DatFinal)[n] <- List[n]
  break
}
List00 <- unique(MeshOB01$Object)
x <- x + 1
}
}

return(DatFinal)

}


