if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
#if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("readr")){install.packages("readr")}; library(readr)


################################################
###  Create the Graph Tree
################################################
#Data = B

treeGraph <- function(Data = Dat, Out_Rename=TRUE){
  rownames(Data) <- 1:nrow(Data)
  #head(Data)
  if(any(colnames(Data) == "subjectLabel") && any(colnames(Data) == "parentClassLabel")){
  Data00 <- Data[,c(2,4)]
  #head(Data00)
  LEN <- unique(unlist(Data[,c(2,4)]))
  for(m in seq_len(length(LEN))){
  #m <- 1
  Data00[Data00 == LEN[m]] <- paste("SP_", formatC(m, flag="0", width=6), sep="")
  }
  LENlist <- data.frame(NAME=LEN, SP=paste("SP_", formatC(seq_len(length(LEN)), flag="0", width=6), sep=""))
  #head(LENlist)
  
  colnames(Data00) <- c("subjectLabel_SP", "parentClassLabel_SP")
  Data01 <- data.frame(Data[,c(2,4)], Data00)
  #head(Data)
  #head(Data01)
  Data02 <- data.frame(Data01, Data[,6:8])
  
  #head(Data02)
  FIN <- Data02$subjectLabel_SP[1]
  eval(parse(text = paste('"', FIN, '"', ' <- Node$new("', FIN, '")', sep="")))
  
  for(n in seq_len(nrow(Data02))){
    if(any(colnames(Data02) == "UpDown")){
    if(Data02$UpDown[n] == "up"){
      try(eval(parse(text = paste('"', Data02$parentClassLabel_SP[n], '"', ' <- ', Data02$subjectLabel_SP[n], '$AddChild("', Data02$parentClassLabel_SP[n], '")', sep=""))), silent=T)
    }else{
      try(eval(parse(text = paste('"', Data02$subjectLabel_SP[n], '"', ' <- ', Data02$parentClassLabel_SP[n], '$AddChild("', Data02$subjectLabel_SP[n], '")', sep=""))), silent=T)
    }}else{
      try(eval(parse(text = paste('"', Data02$parentClassLabel_SP[n], '"', ' <- ', Data02$subjectLabel_SP[n], '$AddChild("', Data02$parentClassLabel_SP[n], '")', sep=""))), silent=T)
  }}
  
  if(Out_Rename){
  eval(parse(text = paste('readr::write_excel_csv(data.frame(', FIN, '), paste("./treeGraph", ".txt", sep=""), col_names = F)')))
  eval(parse(text = paste('readr::write_excel_csv(data.frame(LENlist), paste("./treeGraph_list", ".csv", sep=""), col_names = F)')))
    
  for(l in seq_len(nrow(LENlist))){
  #l <- 2
  try(system(paste('sed -i ".bak" "s/', LENlist[l,2], '/', LENlist[l,1], '/g" treeGraph.txt' , sep="")), silent=TRUE)
  }
  eval(parse(text = paste('return(', FIN, ')', sep="")))
  }else{
    eval(parse(text = paste('return(', FIN, ')', sep="")))
  }}else{
    return(NULL)
}}

