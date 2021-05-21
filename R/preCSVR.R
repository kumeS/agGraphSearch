##' @title file processing
##' @author Satoshi Kume
##'
##' @export preCSVR
##'
##'

#File_path="./00_Input/Intermdediates_TermDict.csv"

preCSVR <- function(File_path, Nlines=1){

if(!grepl(".csv$", File_path)){return(message("Warning: Not proper value of File_path"))}
con_file <- file(description = File_path, open = "r")

x <- 0
N <- Nlines

while( TRUE ){
x <- x + 1
try(a <- readLines(con_file, n = N), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

a1 <- sub("[,]$", "", a)
a1 <- sub("[,]$", "", a1)
a1 <- sub("[,]$", "", a1)
a2 <- data.frame(X=a1)
#head(a2)
if(x == 1){
readr::write_lines(a2,
                   file=sub(".csv$", "_preCSVR.csv", File_path),
                   append=FALSE)
}else{
readr::write_lines(a2,
                   file=sub(".csv$", "_preCSVR.csv", File_path),
                   append=TRUE)
}
}
}

