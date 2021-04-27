##' @title Interactive html table using formattable package.
##'
##' @param Data an input as a data.frame.
##' @param Output logical; if TRUE, create the output file as a name of File.
##' @param File a string of file name.
##' @param Browse logical.
##'
##' @description this function creates a interactive table for checking the data.
##'
##' @return formattable::formattable
##' @author Satoshi Kume
##' @export agTableFM
##' @importFrom formattable formattable
##' @importFrom htmlwidgets saveWidget
##' @importFrom formattable as.htmlwidget
##' @importFrom formattable normalize_bar
##' @examples \dontrun{
##'
##' print(iris)
##'
##' #run
##' agTableFM( Data = iris )
##'
##' }

agTableFM <- function(Data,
                      Output=TRUE,
                      Browse=TRUE,
                      File=paste0("agTableFM_table_", format(Sys.time(), "%y%m%d_%H%M"),".html")
                      ){

DTtable <- formattable::formattable(Data,
                                    list(count = formattable::normalize_bar("pink")))

if(Output){
  DTtable %>% formattable::as.htmlwidget() %>% htmlwidgets::saveWidget(file = File)
  if(Browse){ browseURL(File) }
  return(DTtable)
}else{
  return(DTtable)
}}




