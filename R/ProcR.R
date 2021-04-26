##' @title Bind list to Data Frame
##'
##' @param input A list of data frames. their colnames should be same.
##'
##' @description
##' Create a data frame from a list of multiple data frames with the same colnames.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export ListDF2DF
##' @import magrittr
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

ListDF2DF <- function(input){

if(!is.list(input)){return(message("Warning: Not proper value of input"))}
if(!is.data.frame(input[[1]])){return(message("Warning: Not proper value of input"))}

a <- c()
for(n in base::seq_len(length(input))){
a <- a %>% base::rbind(input[[n]])
}

return(data.frame(a, stringsAsFactors = F))

}

########################################
#After agWD_Alt
########################################
#Check NA or not
checkNA_af_agWD_Alt <- function(input){
a <- table(names(unlist(purrr::map(input, function(x) table(is.na(x))))))
return(a)
}

#Check nrow
checkNrow_af_agWD_Alt <- function(input){
a <- table(names(unlist(purrr::map(input, function(x) table(nrow(x))))))
return(a)
}

##Exclude duplicates by ID with the colname of subject.
Exclude_duplicates <- function(input, conNum){

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








