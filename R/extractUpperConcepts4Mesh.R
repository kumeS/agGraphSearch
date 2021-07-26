##' @title The upper class search of Mesh RDF
##'
##' @param Lab_List a character vector of input.
##' @param Data a RDF data.
##' @param Labels a RDF data.
##' @param broaderProperty a string vector of property.
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
                                      broaderProperty){

#Parameters
Dat <- c()
List <- Lab_List
MeshOthers_broader <- Data
MeshLabels01 <- Labels

x <- 1
if(!is.vector(Lab_List)){ return(message("Warning: Not proper value of Lab_List")) }
if(!is.vector(broaderProperty)){ return(message("Warning: Not proper value of broaderProperty")) }

if(!all(colnames(MeshOthers_broader) %in% c("Subject","Property","Object","OtherInfo"))){
   MeshOthers_broader <- MeshOthers_broader[,colnames(MeshOthers_broader) %in% c("Subject","Property","Object","OtherInfo")]
}

if(!all(colnames(MeshOthers_broader) == c("Subject","Property","Object","OtherInfo"))){
   return(message("Warning: Not proper value of Data"))
}

MeshOthers_broader00 <- MeshOthers_broader[c(MeshOthers_broader$Property %in% broaderProperty),]

repeat{
print(x)
MeshOthers_broader01 <- MeshOthers_broader00[c(MeshOthers_broader00$Subject %in% List),]
if(nrow(MeshOthers_broader01) != 0){
  Dat <- Dat %>% rbind(MeshOthers_broader01)
}else{
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Object)
  rownames(Dat) <- 1:nrow(Dat)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
  Dat <- Dat[,-ncol(Dat)]
  if(ncol(Dat) == 4){colnames(Dat) <- c("subject", "property", "parentClass", "OtherInfo")}
  Dat <- Dat[order(Dat$subject, decreasing = T),]
  if(any(colnames(MeshLabels01) == c("Subject", "Object"))){
    Dat00 <-  merge(Dat, MeshLabels01, by.x = "subject", by.y = "Subject", all = F, sort = F)
    Dat01 <-  merge(Dat00, MeshLabels01, by.x = "parentClass", by.y = "Subject", all = F, sort = F)
    colnames(Dat01)[c((ncol(Dat01)-1):ncol(Dat01))] <- c("subjectLabel", "parentClassLabel")
    Dat02 <- Dat01[,c("subject", "property",
                  "parentClass", "OtherInfo",
                  "subjectLabel", "parentClassLabel")]
  }
  return(Dat02)
}
List <- unique(MeshOthers_broader01$Object)
x <- x + 1
}
}


