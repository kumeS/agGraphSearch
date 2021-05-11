##' @title Visualize the results for common upper-level entities
##' @return data.frame
##' @author Satoshi Kume
##' @export pc_plot
##' @importFrom plotly plot_ly
##' @importFrom plotly layout
##' @importFrom plotly as_widget
##' @importFrom magrittr %>%
##' @importFrom htmlwidgets saveWidget
##' @importFrom filesstrings file.move
##' @examples \dontrun{
##' }

pc_plot <- function(input, SaveFolder, FileName, IDnum){
if(!is.data.frame(input)){return(message("Warning: Not proper value of input"))}
if(!any(colnames(input) == "parentClassLabel")){return(message("Warning: Not proper value of input"))}
if(!any(colnames(input) == "Freq")){return(message("Warning: Not proper value of input"))}

input <- input[order(-input$Freq),]
Data <- data.frame(V1=input$parentClassLabel,
                   V2=as.numeric(input$Freq),
                   V3=1:nrow(input))

fig <- plotly::plot_ly(Data, x = ~V3, y = ~V2, type = 'bar', name = '',
               hovertemplate = paste(Data$V1, '<br>%{y}<br>', sep=""),
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% plotly::layout(title = paste0("Frequency"),
                              yaxis = list(title = 'Count'),
                              xaxis = list(title = ""),
                              xaxis = Data$V1)

#save
htmlwidgets::saveWidget(plotly::as_widget(fig), FileName )
filesstrings::file.move(files=FileName,
                        destinations=SaveFolder,
                        overwrite = TRUE)

#Correction by number of labels
if(!is.numeric(IDnum)){return(message("Warning: Not proper value of IDnum"))}
Data <- data.frame(V1=input$parentClassLabel,
                   V2=round(as.numeric(input$Freq)/IDnum*100, 2),
                   V3=1:nrow(input))
fig <- plotly::plot_ly(Data, x = ~V3, y = ~V2, type = 'bar', name = '',
                       hovertemplate = paste(Data$V1, '<br>%{y}<br>', sep=""),
                       marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% plotly::layout(title = paste0("Percent (%)"),
                              xaxis = list(title = ""),
                              yaxis = list(title = 'Percent', range = c(0, 100)))

#save
FileName0 <- paste0(sub(".html", "", FileName), "_percent.html")
htmlwidgets::saveWidget(plotly::as_widget(fig), FileName0 )
filesstrings::file.move(files=FileName0,
                        destinations=SaveFolder,
                        overwrite = TRUE)
}


