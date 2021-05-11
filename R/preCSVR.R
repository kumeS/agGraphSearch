
#File_path="./00_Input/Intermdediates_TermDict.csv"

preCSVR <- function(File_path){

if(!grepl(".csv$", File_path)){return(message("Warning: Not proper value of File_path"))}
con_file <- file(description = File_path, open = "r")

x <- 0
N <- 10000

while( TRUE ){
x <- x + 1
try(a <- readLines(con_file, n = N), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

a1 <- sub("[,]$", "", a)
a1 <- sub("[,]$", "", a1)
a1 <- sub("[,]$", "", a1)
a2 <- data.frame(X=a1)

readr::write_excel_csv(a2,
                       file=sub(".csv$", "_preCSVR.csv", File_path),
                       append=TRUE, col_names = FALSE, )
}
}




