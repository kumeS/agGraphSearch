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
  VIS <-  visNetwork::visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visNetwork::visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>%
      visNetwork::visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visNetwork::visIgraphLayout(layout = "layout_with_fr") %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T)
  }else{
    VIS <-  visNetwork::visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visNetwork::visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>%
      visNetwork::visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visNetwork::visIgraphLayout(layout = "layout_with_fr") %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T)
  }
  if(outputNodesEdges){
    readr::write_excel_csv(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") )
    readr::write_excel_csv(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") )
  }else{}
  if(output){
    VIS %>% networkD3::saveNetwork(file = file)
    if(Browse){browseURL(file)}else{return(NULL)}
  } else {}
  return(VIS)
}


