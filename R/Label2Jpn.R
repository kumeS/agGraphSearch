##' @title Translate labels to Japanese
##'
##' @param Data an input data
##' @param Auth_Key a string, Your Authentication Key
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export Label2Jpn
##'

Label2Jpn <- function(Data, Auth_Key="Your Authentication Key"){
#head(words)
words <- Data

if(!any(colnames(words) == "subjectLabel")){return(message("Warning: Not proper value of subjectLabel"))}
if(!any(colnames(words) == "parentClassLabel")){return("Warning: Not proper value of parentClassLabel")}
wordsList <- unique(c(words$subjectLabel, words$parentClassLabel))
k <- length(wordsList)
Mat <- words[,c("subjectLabel","parentClassLabel")]

for(n in 1:k){
#n <- 1
message(colourise(paste(n, "/", k), fg = "blue", bg = NULL))
if(grepl("[,][ ]", wordsList[n]) & !grepl("[,][ ]and", wordsList[n])){
a1 <- strsplit(wordsList[n], split = ", ")[[1]]
aa <- paste(a1[length(a1):1], collapse = " ")
}else{
aa <-  wordsList[n]
}

#Translate to Japanese
a <- wordsList[n]
b  <- DeePL(aa, Auth_Key=Auth_Key)
Mat[Mat == wordsList[n]] <- b
message(colourise(paste("  ", a, "=>", b), fg = "green", bg = NULL))
}

words[,c("subjectLabel","parentClassLabel")] <- Mat
return(words)

}


