##' @title Bind list to Data Frame
##'
##' @param input A list of data frames. their colnames should be same.
##' @param NA.omit logical: perform na.omit() or not after combining the data.
##' @description
##' Create a data frame from a list of multiple data frames with the same colnames.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export ListDF2DF
##' @importFrom magrittr %>%
##' @examples \dontrun{
##' #parameters
##'
##' #test dat
##' test <- list(data.frame(x=c(1:3), y=c(1:3)),
##'              data.frame(x=c(4:6), y=c(4:6)),
##'              data.frame(x=c(7:20), y=c(7:20)))
##'

##' #run
##' ListDF2DF( input=test )
##'
##' }

ListDF2DF <- function(input, NA.omit=FALSE){

if(!is.list(input)){return(message("Warning: Not proper value of input"))}
if(!is.data.frame(input[[1]])){return(message("Warning: Not proper value of input"))}

a <- c()
for(n in base::seq_len(length(input))){
a <- a %>% base::rbind(input[[n]])
}

if(NA.omit){a <- stats::na.omit(a)}

rownames(a) <- 1:nrow(a)
return(data.frame(a, stringsAsFactors = F))

}

########################################
#After agWD_Alt
########################################
#Check NA or not
checkNA_af_agWD_Alt <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- table(names(unlist(purrr::map(input, function(x) table(is.na(x))))))
return(a)
}

#Check nrow
checkNrow_af_agWD_Alt <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- table(names(unlist(purrr::map(input, function(x) table(nrow(x))))))
return(a)
}

########################################
#After agQIDtoLabel
########################################
#Check nrow
checkNrow_af_agQIDtoLabel <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- table(unlist(purrr::map(input, function(x){nrow(x)})))
return(a)
}

checkNrowVec_af_agQIDtoLabel <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- unlist(purrr::map(input, function(x){nrow(x)}))
return(a)
}

########################################
#After agCount_Label_Num
########################################
checkHitLabel_af_agCount_Label_Num <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- unlist(purrr::map(input, function(x){ x$Hit_Label }))
return(a)
}

checkHitALL_af_agCount_Label_Num <- function(input){
if(!is.list(input)){return(message("Warning: Not proper value of input"))}
a <- unlist(purrr::map(input, function(x){ x$Hit_ALL }))
return(a)
}

########################################
##Exclude duplicates by ID with the colname of subject.
########################################
Exclude_Subject_duplicates <- function(input, conNum){

if(any(colnames(input) == "subject")){
if(c(1:ncol(input))[colnames(input) == "subject"] != conNum){
  return(message("Warning: Not proper value of conNum"))
}}

colnames(input)[conNum] <- "subject"
cc <- input["subject"] %>%
  unique() %>%
  rownames() %>%
  as.numeric()

a <- input[cc,]
return(a)

}

########################################
##Exclude duplicates by ID with the colname of subject.
########################################
#library(magrittr)

Exclude_Graph_duplicates <- function(input, conNum="Last"){
if(!is.data.frame(input)){ return(message("Warning: Not proper value of input")) }
if(nrow(input) == 0){ return(message("Warning: Not proper value of input")) }
if(dim(input)[2] != 11){ return(message("Warning: Not proper value of input")) }
if(conNum == "Last"){
conNum0 <- ncol(input)
conName <- colnames(input)[ncol(input)]
}else{
if(is.numeric(conNum)){
  conNum0 <- conNum
  conName <- colnames(input)[conNum]
}else{
  return(message("Warning: Not proper value of conNum"))
}}

cc <- input[conName] %>%
  unique() %>%
  rownames() %>%
  as.numeric()

a <- input[cc,]
rownames(a) <- 1:nrow(a)

return(a)

}

Exclude_duplicates <- function(input, conNum="Last"){
if(!is.data.frame(input)){ return(message("Warning: Not proper value of input")) }
if(nrow(input) == 0){ return(message("Warning: Not proper value of input")) }
if(conNum == "Last"){
conNum0 <- ncol(input)
conName <- colnames(input)[ncol(input)]
}else{
if(is.numeric(conNum)){
  conNum0 <- conNum
  conName <- colnames(input)[conNum]
}else{
  return(message("Warning: Not proper value of conNum"))
}}

cc <- input[conName] %>%
  unique() %>%
  rownames() %>%
  as.numeric()

a <- input[cc,]
rownames(a) <- 1:nrow(a)

return(a)

}

Cutoff_FreqNum <- function(input1, input2, By="parentClass", Sort="Freq", FreqNum=2){
if(!is.data.frame(input1)){ return(message("Warning: Not proper value of input1")) }
if(!is.data.frame(input2)){ return(message("Warning: Not proper value of input2")) }

a <- merge(input1, input2, by=By, all = T, sort = F)
if(!all(is.numeric(unlist(a[Sort], use.names=FALSE)))){return(message("Warning: Not proper value of Sort"))}

b <- a[order(a[Sort]),]
d <- b[b[Sort] >= FreqNum,]
rownames(d) <- 1:nrow(d)

return(d)

}

########################################
#After agCount_Label_Num
########################################
#Check nrow
checkNrow_af_agCount_Label_Num <- function(input){
a <- unlist(purrr::map(input, function(x){nrow(x)}))
return(a)
}

checkColumn_af_agCount_Label_Num <- function(input, colName="Hit_subClassOf_Instance"){
a <- unlist(purrr::map(input, function(x){ eval(parse(text = paste0("x$", colName))) }))
return(a)
}

##' @title Check list data
##' @author Satoshi Kume
##'
##' @export checkNrow_af
##' @export checkColumn_af
##' @export checkLoop_af
##' @export removeLoop_af
##' @export Exclude_Graph_duplicates
##' @export Exclude_Subject_duplicates
##' @export Exclude_duplicates
##' @export Cutoff_FreqNum
##' @export countCommonEntities
##'


#Check nrow
checkNrow_af <- function(input){
a <- unlist(purrr::map(input, function(x){nrow(x)}))
return(a)
}

checkColumn_af <- function(input, colName="Hit_subClassOf_Instance"){
a <- unlist(purrr::map(input, function(x){ eval(parse(text = paste0("x$", colName))) }))
return(a)
}

checkLoop_af <- function(input, col1=3,  col2=5){
a <- unlist(purrr::map(input, function(x){ sum(x[,col1] == x[,col2]) }))
return(a)
}


removeLoop_af <- function(input, col1=3,  col2=5){
input0 <- input
for(n in seq_len(length(input0))){
input0[[n]] <- input0[[n]][input0[[n]][,col1] != input0[[n]][,col2],]
}
return(input0)
}

countCommonEntities <- function(upEntity){
Count_upEntity <- table(upEntity)
Count_upEntity_DF <- data.frame(parentClass=names(Count_upEntity),
                                Freq=as.numeric(Count_upEntity),
                                row.names = 1:length(Count_upEntity),
                                stringsAsFactors = F)
return(Count_upEntity_DF)
}

