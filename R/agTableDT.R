##' @title Interactive html table using DT package.
##'
##' @param Data an input as a data.frame.
##' @param Output logical; if TRUE, create the output file as a name of File.
##' @param File a string of file name.
##' @param Rownames logical.
##' @param AutoWidth logical.
##' @param Browse logical.
##' @param Editable logical.
##' @param Caption a string of caption.
##' @param Width a string of px; ex. '300px'.
##' @param PageLength a numeric vector.
##'
##' @description this function creates a interactive table for checking the data.
##'
##' @return DT::datatable
##' @author Satoshi Kume
##' @export agTableDT
##' @importFrom DT datatable
##' @importFrom htmlwidgets saveWidget
##' @importFrom magrittr %>%
##'
##' @examples \dontrun{
##'
##' head(iris)
##'
##' #run
##' agTableDT( Data = iris, PageLength = 10 )
##'
##' }

agTableDT <- function(Data,
                      Output=TRUE,
                      File=paste0("agTableDT_table_", format(Sys.time(), "%y%m%d_%H%M"),".html"),
                      Rownames=FALSE,
                      AutoWidth=TRUE,
                      Browse=TRUE,
                      Editable=FALSE,
                      Caption="Table. results",
                      Width='100px',
                      PageLength=25){

if(!is.data.frame(Data)){return(message("Warning: Not proper value of Data"))}

targets <- c(0:(ncol(Data)-1))
DTtable <- DT::datatable(Data,
                         rownames = Rownames,
                         caption = Caption,
                         class = 'cell-border stripe',
                         filter = 'top',
                         options = list(autoWidth = AutoWidth,
                                        pageLength = PageLength,
                                        lengthMenu = c(5, 10, 15, 20, 25, 30,
                                                       40, 50, 75, 100, 125, 150,
                                                       200, 250, 300, 400, 500,
                                                       1000, 2000),
                                        searchHighlight = TRUE,
                                        scrollX = TRUE,
                                        fixedColumns = TRUE,
                                        scrollCollapse = TRUE,
                                        columnDefs = list(list( targets=targets, width = Width))
                                        ),
                         editable = Editable)
if(Output){
  DTtable %>% htmlwidgets::saveWidget(file = File)
  if(Browse){ browseURL(File) }
  return(DTtable)
} else{
  return(DTtable)
}}


