##' @title File name
##'
##' @param FirstWords a first string of output.
##' @param extension a character of file extension.
##'
##' @return character
##' @author Satoshi Kume
##' @export paste1
##'


paste1 <- function(FirstWords, extension="html"){

FileName <- paste0(FirstWords, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".", extension)

return(FileName)

}

