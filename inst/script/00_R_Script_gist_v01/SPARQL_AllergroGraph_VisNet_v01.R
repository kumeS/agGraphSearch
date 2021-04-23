if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
#if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("readr")){install.packages("readr")}; library(readr)


################################################
###  Create the network graphs
################################################
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

#Graph=GraphData2; Count=2; Size=10; SmallSize=5; FontSize=7; StarSize=10; SEED=123; Selected=NULL; output=FALSE; file="Wikidat_visNet01.html"; HeightSclale = "750px"; WidthSclale = "110%"


## visNetwork version
agVisNetwork <- function(Graph=GraphData, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                           SEED=123, Selected=NULL, Browse=FALSE, output=FALSE, file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE){
  if(is.null(Graph)){return("NULL")}
  set.seed(SEED)

  #if(nrow(Graph) > 5000){message("Too huge data"); break()}
  if(!any(colnames(Graph) == "propertyLabel")){
    if(any(colnames(Graph) == "parentClassLabel")){
      Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
      Graph$parentClassLabel <- paste0(Graph$parentClassLabel, ".", Graph$parentClass)
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$parentClassLabel, Graph$parentClassLabel), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel)
    } else { 
      Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
      Graph$childClassLabel <- paste0(Graph$childClassLabel, ".", Graph$childClass)
      LABEL <- c(Graph$subjectLabel, Graph$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$childClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$childClassLabel, to = Graph$subjectLabel)
    }
  } else {
    if(any(colnames(Graph) == "parentClassLabel")){
      Graph$subjectLabel <- paste0( Graph$subjectLabel, ".",Graph$subject)
      Graph$parentClassLabel <- paste0(Graph$parentClassLabel, ".", Graph$parentClass)
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", 
                          color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, 
                          stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel,
                          dashes = c(ifelse(Graph$property == "http://www.wikidata.org/entity/P31", TRUE, FALSE)))
      
      edges1 <- edges[,2:3]; colnames(edges1) <- c("id", "shape")
      edges2 <- edges1[as.numeric(as.character(rownames(unique(edges1['id'])))),]
      nodes1 <- merge(nodes, edges2, by="id", all = T, sort = F)
      nodes1$shape[is.na(nodes1$shape)] <- "FALSE"
      nodes1$shape <- ifelse(nodes1$shape, "star", "dot")
      nodes <- nodes1
    } else {
      Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
      Graph$childClassLabel <- paste0(Graph$childClassLabel, ".", Graph$childClass)
      Graph1 <- Graph[Graph$property == "http://www.wikidata.org/entity/P279",] 
      LABEL <- c(Graph1$subjectLabel, Graph1$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph1$subjectLabel, Graph1$childClassLabel), group = c(Graph1$group, Graph1$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, shape="dot", stringsAsFactors = F)
      edges <- data.frame(from = Graph1$childClassLabel, to = Graph1$subjectLabel, dashes=FALSE)
      
      Graph2 <- Graph[Graph$property != "http://www.wikidata.org/entity/P279",]
      LABEL2 <- c(Graph2$childClassLabel)
      LABEL2 <- data.frame(id = unique(LABEL2), stringsAsFactors = F)
      Group2 <- data.frame(id = c(Graph2$childClassLabel), group = c(Graph2$group), stringsAsFactors = F)
      Group3 <- Group2[as.numeric(as.character(rownames(unique(Group2['id'])))),]
      LABEL2 <- merge(LABEL2, Group3, by="id", sort=F)
      nodes2 <- data.frame(id = LABEL2$id, label = LABEL2$id, group=LABEL2$group, color.background="lightyellow", color.border="goldenrod1", color.highlight="orangered", size=StarSize, font.size =FontSize, shape="star", stringsAsFactors = F)
      edges2 <- data.frame(from = Graph2$subjectLabel, to = Graph2$childClassLabel, dashes=TRUE)

      edges %>% rbind(edges2) -> edges
      }
  }
  
if(Count > 1){
    Count10 <- table(nodes$group)[order(table(nodes$group))]
    group2 <- nodes$group
    id2 <- nodes$id
    if(NodeColorRandom){
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    }else{
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))
    }
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        #n <- 241
        nodes$color.background[group2 == names(Count10)[n]] <- ColorSpa[n]
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
        nodes$size[group2 == names(Count10)[n]] <- Size
     }else{
        #nodes$size[group2 == names(Count10)[n]] <- SmallSize
        #nodes$group[group2 == names(Count10)[n]] <- "Others"
     }}
  }else{}
  
  if(any(colnames(Graph) == "parentClassLabel")){
    if(Count > 1){
      for(n in seq_len(length(Count10))){
        if(Count10[n] > Count){
          #n <- 241
          #head(nodes)
          nodes$color.background[id2 == names(Count10)[n]] <- ColorSpa[n]
          nodes$color.border[id2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
          nodes$size[id2 == names(Count10)[n]] <- Size
          nodes$group[id2 == names(Count10)[n]] <- names(Count10)[n]
        }else{}}
    }else{}
  }else{}

  if(!any(colnames(Graph) == "propertyLabel")){}else{
    if(any(colnames(Graph) == "parentClassLabel")){}else{
      nodes %>% rbind(nodes2) -> nodes}}
  nodes <- nodes[as.numeric(as.character(rownames(unique(nodes['id'])))),]
  
  if(is.null(Selected)){
  VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(outputNodesEdges){
    
    readr::write_excel_csv(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") ) 
    readr::write_excel_csv(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") ) 
  }else{}
  if(output){
    VIS %>% saveNetwork(file = file)
    if(Browse){browseURL(file)}else{return(NULL)}
  } else {}
  return(VIS)
}

###################################################################
###################################################################
## visNetwork version
#Graph=B; NodeColorRandom=F; Count=2; Size=10; SmallSize=5; StarSize=10; FontSize=7; HeightSclale = "750px"; WidthSclale = "110%"
#Common=list2b; Search=list1a;CommonExpand=list3;SEED=123; Selected=NULL; Browse=FALSE; output=FALSE; file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""); outputNodesEdges=FALSE



agVisNetwork_cr <- function(Graph=GraphData, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                              Common=NULL, Search=NULL,CommonExpand=NULL,Compounds=NULL,
                           SEED=123, Selected=NULL, Browse=FALSE, output=FALSE, file=paste("Wikidat_visNet", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE){
  if(is.null(Graph)){return("NULL")}
  set.seed(SEED)
  #if(nrow(Graph) > 5000){message("Too huge data"); break()}
  if(!any(colnames(Graph) == "propertyLabel")){
    if(any(colnames(Graph) == "parentClassLabel")){
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$parentClassLabel, Graph$parentClassLabel), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel)
    } else { 
      LABEL <- c(Graph$subjectLabel, Graph$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$childClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, stringsAsFactors = F)
      edges <- data.frame(from = Graph$childClassLabel, to = Graph$subjectLabel)
    }
  } else {
    if(any(colnames(Graph) == "parentClassLabel")){
      LABEL <- unique(c(Graph$subjectLabel, Graph$parentClassLabel))
      Group <- data.frame(id = c(Graph$subjectLabel, Graph$parentClassLabel), group = c(Graph$group, Graph$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      nodes <- data.frame(id = LABEL, label = LABEL, group=Group1$group, color.background="lightblue", 
                          color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, 
                          stringsAsFactors = F)
      edges <- data.frame(from = Graph$subjectLabel, to = Graph$parentClassLabel,
                          dashes = c(ifelse(Graph$property == "http://www.wikidata.org/entity/P31", TRUE, FALSE)))
      
      edges1 <- edges[,2:3]; colnames(edges1) <- c("id", "shape")
      edges2 <- edges1[as.numeric(as.character(rownames(unique(edges1['id'])))),]
      nodes1 <- merge(nodes, edges2, by="id", all = T, sort = F)
      nodes1$shape[is.na(nodes1$shape)] <- "FALSE"
      nodes1$shape <- ifelse(nodes1$shape, "star", "dot")
      nodes <- nodes1
    } else { 
      Graph1 <- Graph[Graph$property == "http://www.wikidata.org/entity/P279",] 
      LABEL <- c(Graph1$subjectLabel, Graph1$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph1$subjectLabel, Graph1$childClassLabel), group = c(Graph1$group, Graph1$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, shape="dot", stringsAsFactors = F)
      edges <- data.frame(from = Graph1$childClassLabel, to = Graph1$subjectLabel, dashes=FALSE)
      
      Graph2 <- Graph[Graph$property != "http://www.wikidata.org/entity/P279",]
      LABEL2 <- c(Graph2$childClassLabel)
      LABEL2 <- data.frame(id = unique(LABEL2), stringsAsFactors = F)
      Group2 <- data.frame(id = c(Graph2$childClassLabel), group = c(Graph2$group), stringsAsFactors = F)
      Group3 <- Group2[as.numeric(as.character(rownames(unique(Group2['id'])))),]
      LABEL2 <- merge(LABEL2, Group3, by="id", sort=F)
      nodes2 <- data.frame(id = LABEL2$id, label = LABEL2$id, group=LABEL2$group, color.background="lightyellow", color.border="goldenrod1", color.highlight="orangered", size=StarSize, font.size =FontSize, shape="star", stringsAsFactors = F)
      edges2 <- data.frame(from = Graph2$subjectLabel, to = Graph2$childClassLabel, dashes=TRUE)

      edges %>% rbind(edges2) -> edges
      }
  }
  
if(Count > 1){
    Count10 <- table(nodes$group)[order(table(nodes$group))]
    group2 <- nodes$group
    id2 <- nodes$id
    if(NodeColorRandom){
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    }else{
      ColorSpa <- colorspace::rainbow_hcl(length(Count10))
    }
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        #n <- 241
        nodes$color.background[group2 == names(Count10)[n]] <- ColorSpa[n]
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
        nodes$size[group2 == names(Count10)[n]] <- Size
     }else{
        #nodes$size[group2 == names(Count10)[n]] <- SmallSize
        #nodes$group[group2 == names(Count10)[n]] <- "Others"
     }}
  }else{}
  
  if(any(colnames(Graph) == "parentClassLabel")){
    if(Count > 1){
      for(n in seq_len(length(Count10))){
        if(Count10[n] > Count){
          #n <- 241
          #head(nodes)
          nodes$color.background[id2 == names(Count10)[n]] <- ColorSpa[n]
          nodes$color.border[id2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
          nodes$size[id2 == names(Count10)[n]] <- Size
          nodes$group[id2 == names(Count10)[n]] <- names(Count10)[n]
        }else{}}
    }else{}
  }else{}

  if(!any(colnames(Graph) == "propertyLabel")){}else{
    if(any(colnames(Graph) == "parentClassLabel")){}else{
      nodes %>% rbind(nodes2) -> nodes}}
  nodes <- nodes[as.numeric(as.character(rownames(unique(nodes['id'])))),]
  
  #head(nodes)
  if(!is.null(Search)){
  NNN <- paste0("wd:", unlist(map(nodes$id, function(x) strsplit(x , ".wd:")[[1]][2])))
  nodes$color.background[NNN %in% Search] <- "skyblue"
  nodes$size[NNN %in% Search] <- Size*2  
  }
  if(!is.null(Common)){
  NNN <- paste0("wd:", unlist(map(nodes$id, function(x) strsplit(x , ".wd:")[[1]][2])))
  nodes$color.background[NNN %in% Common] <- "lightgreen"
  nodes$size[NNN %in% Common] <- Size*2  
  }
  if(!is.null(CommonExpand)){
  NNN <- paste0("wd:", unlist(map(nodes$id, function(x) strsplit(x , ".wd:")[[1]][2])))
  nodes$color.background[NNN %in% CommonExpand] <- "yellow"
  nodes$size[NNN %in% CommonExpand] <- Size*2  
  }  

  if(is.null(Selected)){
  VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(outputNodesEdges){
    readr::write_excel_csv(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") ) 
    readr::write_excel_csv(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") ) 
  }else{}
  if(output){
    VIS %>% saveNetwork(file = file)
    if(Browse){browseURL(file)}else{return(NULL)}
  } else {}
  return(VIS)
}

###################################################################
###################################################################
agVisNetworkFromCSV <- function(Nodes="nodes.csv", Edges="edges.csv", 
                                  Count=2, Size=10, SmallSize=5,
                                  FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
                                  SEED=123, Selected=NULL, output=FALSE, 
                                  file=paste0("Wikidat_visNet_", format(Sys.time(), "%y%m%d_%H%M"),".html")){
  nodes <- read.table(file=Nodes, sep=",", header=T, stringsAsFactors =F)
  edges <- read.table(file=Edges, sep=",", header=T, stringsAsFactors =F)
  
  nodes$size <- Size
  nodes$font.size <- FontSize
  nodes$color.background <- "lightblue"
  
  if(Count > 1){
    Count10 <- table(nodes$group)
    group2 <- nodes$group
    ColorSpa <- colorspace::rainbow_hcl(length(Count10))[sample(1:length(Count10), length(Count10), replace = FALSE)]
    for(n in 1:length(Count10)){
      if(Count10[n] > Count){
        nodes$color.background[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1)
        nodes$color.border[group2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
      }else{
        nodes$size[group2 == names(Count10)[n]] <- SmallSize
        nodes$group[group2 == names(Count10)[n]] <- "Others"
      }}
  }else{}
  
  nodes$size[group2 == "Others"] <- SmallSize
  nodes$color.background[group2 == "Others"] <- "lightblue"
  
  set.seed(SEED)
  if(is.null(Selected)){
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T) 
  }else{
    VIS <-  visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>% 
      visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T) 
  }
  if(output){
    VIS %>% saveNetwork(file = file)
    browseURL(file)
  } else {}
  return(VIS)
}

################################################
###  Create the Table
################################################
agGraphDT <- function(Data=GraphData, output=FALSE, file=paste("DT.table_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), Rownames=FALSE, AutoWidth=TRUE, targets=c(0:(ncol(Data)-1)), width='300px'){
  DTtable <-   DT::datatable(Data,
                             rownames = Rownames,
                             caption = "Table. SPARQL results",
                             class = 'cell-border stripe',
                             filter = 'top',
                             options = list(autoWidth = AutoWidth,
                                            pageLength = 25, 
                                            lengthMenu = c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 1000, 2000),
                                            searchHighlight = TRUE,
                                            scrollX = TRUE,
                                            fixedColumns = TRUE,
                                            scrollCollapse = TRUE,
                                            columnDefs = list(list( targets=targets, width = width))
                                            ), 
                             editable = FALSE) 
  if(output){
    DTtable %>% 
    htmlwidgets::saveWidget(file = file)
    browseURL(file)
    return(DTtable)
  } else{
    return(DTtable)
  }}


GraphForm <- function(Data=GraphData, output=FALSE, file=paste("FOR.table_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep="")){
  DTtable <- formattable(Data, list(count = normalize_bar("pink")))
  if(output){
    DTtable %>% 
      as.htmlwidget() %>%
      htmlwidgets::saveWidget(file = file)
    browseURL(file)
    return(DTtable)
  } else {return(DTtable)}}
  
