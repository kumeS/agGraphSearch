##' @title The upper class search of Mesh RDF
##'
##' @param Lab_List a character vector of input.
##' @param Data a RDF data.
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
                                      broaderProperty){

#Parameters
Dat <- c()
List <- Lab_List
MeshOthers_broader <- Data
x <- 1
if(!is.vector(Lab_List)){ return(message("Warning: Not proper value of Lab_List")) }
if(!is.vector(broaderProperty)){ return(message("Warning: Not proper value of broaderProperty")) }
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
  return(Dat)
}
List <- unique(MeshOthers_broader01$Object)
x <- x + 1
}
}


