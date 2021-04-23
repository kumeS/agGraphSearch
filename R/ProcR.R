##' @title Bind list to Data Frame
##'
##' @param input A list of data frames. their colnames should be same.
##'
##' @description
##' Create a data frame from a list of multiple data frames with the same colnames.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @import magrittr
##' @export ListDF2DF
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

