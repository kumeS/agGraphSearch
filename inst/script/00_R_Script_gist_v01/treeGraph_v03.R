if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("treemap")){install.packages("treemap")}; library(treemap)
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

