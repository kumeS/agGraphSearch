


## simpleNetwork version
agNetwork3d <- function(Graph = GraphData, output=FALSE, file=paste("Wikidata_Network3d", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""),
                          linkDistance= 40, charge=-30, fontSize=6, opacity=0.8){
    if(any(colnames(Graph) == "parentClassLabel")){
    Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
    Graph$parentClassLabel <- paste0(Graph$parentClassLabel, ".", Graph$parentClass)
    Links <- data.frame(source= Graph$subjectLabel, target=Graph$parentClassLabel, value=NA, stringsAsFactors = F)
    N3d <- simpleNetwork(Links,
                  linkDistance = linkDistance, charge = charge, fontSize = fontSize, fontFamily = "Arial",
                  linkColour = "#666", nodeColour = "#225c87", opacity = opacity, zoom = T)
    }else{
    Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
    Graph$childClassLabel <- paste0(Graph$childClassLabel, ".", Graph$childClass)
    Links <- data.frame(source= Graph$childClassLabel, target=Graph$subjectLabel, value=NA, stringsAsFactors = F)
    N3d <- simpleNetwork(Links,
                  linkDistance = linkDistance, charge = charge, fontSize = fontSize, fontFamily = "Arial",
                  linkColour = "#666", nodeColour = "#225c87", opacity = opacity, zoom = T)
    }

    if(output){
      N3d %>% htmlwidgets::saveWidget(file = file)
      browseURL(file)
    } else {}
    return(N3d)
}




