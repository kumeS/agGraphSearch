##' @title Text table using knitr package.
##'
##' @param Data an input as a data.frame.
##' @param Head logical; if TRUE, show the header only.
##' @param Format a numeric vector.
##' if the value is 1, the output format is "pipe".
##' if the value is 2, the output format is "rst".
##' if the value is 3, the output format is "simple".
##' if the value is 4, the output format is "latex".
##' @param Align a numeric vector.
##' if the value is 1, the output format is "aligned on center".
##' if the value is 2, the output format is "aligned on left".
##' if the value is 3, the output format is "aligned on right".
##' if the value is 4, the output format is "NULL".
##'
##' @description this function creates a text table for checking the data.
##'
##' @return knitr_kable
##' @author Satoshi Kume
##' @export agTableKB
##' @importFrom knitr kable
##' @importFrom magrittr %>%
##' @examples \dontrun{
##'
##' head(iris)
##'
##' #run
##' agTableKB( Data = iris )
##'
##' }

agTableKB <- function(Data,
                      Head=TRUE,
                      Format=1,
                      Align=1){

if(!is.data.frame(Data)){return(message("Warning: Not proper value of Data"))}

base::switch(Format,
             "1" = a <- "pipe",
             "2" = a <- "rst",
             "3" = a <- "simple",
             "4" = a <- "latex",
             return(message("Warning: Not proper value of Format"))
             )

base::switch(Align,
             "1" = b <- "c",
             "2" = b <- "r",
             "3" = b <- "l",
             "4" = b <- NULL,
             return(message("Warning: Not proper value of Align"))
             )


if(Head){
 return(Data %>% head() %>% knitr::kable(format = a, booktabs = T, align = b))
}else{
 return(Data %>% knitr::kable(format = a, booktabs = T, align = b))
}
}

