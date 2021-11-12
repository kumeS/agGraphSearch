##' @title Count triples from a label query via SPARQL.
##'
##' @param Graph a data.frame
##'
##' @return network diagrams
##' @author Satoshi Kume
##' @export agVisNetwork
##' @importFrom visNetwork visNetwork
##' @importFrom visNetwork visEdges
##' @importFrom visNetwork visNodes
##' @importFrom visNetwork visIgraphLayout
##' @importFrom visNetwork visOptions
##' @importFrom networkD3 saveNetwork
##' @importFrom purrr map
##' @importFrom magrittr %>%
##'

## visNetwork version
agVisNetwork <- function(Graph,
                         NodeColorRandom=FALSE,
                         Count=2,
                         StarSize=10,
                         Selected=NULL,
                         Browse=TRUE,
                         Output=FALSE,
                         ColoredSeed=NULL,
                         ColoredTopClass=NULL,
                         MaxRow=100000,
                         Physics=F,
                         Smooth=F,
                         FilePath=paste0("agVisNetwork_", format(Sys.time(), "%y%m%d"),".html"),
                         outputNodesEdges=FALSE){

#Parameters
SmallSize=5; FontSize=7; Size=10; Layout="layout_with_fr"
#options(max.print=100000)

if(is.null(Graph)){return(message("Warning: Not proper value of Graph"))}
if(!is.data.frame(Graph)){return(message("Warning: Not proper value of Graph"))}
if(nrow(Graph) > MaxRow){return(message("Warning: should be less than ", MaxRow, " rows"))}
HeightSclale = "1500px"; WidthSclale = "110%"; SEED=123

set.seed(SEED)

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
                          dashes = c(ifelse(Graph$property == "wdt:P31", TRUE, FALSE)))

      edges1 <- edges[,2:3]; colnames(edges1) <- c("id", "shape")
      edges2 <- edges1[as.numeric(as.character(rownames(unique(edges1['id'])))),]
      nodes1 <- merge(nodes, edges2, by="id", all = T, sort = F)
      nodes1$shape[is.na(nodes1$shape)] <- "FALSE"
      nodes1$shape <- ifelse(nodes1$shape, "star", "dot")
      nodes <- nodes1
    } else {
      Graph$subjectLabel <- paste0(Graph$subjectLabel, ".", Graph$subject)
      Graph$childClassLabel <- paste0(Graph$childClassLabel, ".", Graph$childClass)
      Graph1 <- Graph[Graph$property == "wdt:P279",]
      LABEL <- c(Graph1$subjectLabel, Graph1$childClassLabel)
      LABEL <- data.frame(id = unique(LABEL), stringsAsFactors = F)
      Group <- data.frame(id = c(Graph1$subjectLabel, Graph1$childClassLabel), group = c(Graph1$group, Graph1$group), stringsAsFactors = F)
      Group1 <- Group[as.numeric(as.character(rownames(unique(Group['id'])))),]
      LABEL <- merge(LABEL, Group1, by="id", sort=F)
      nodes <- data.frame(id = LABEL$id, label = LABEL$id, group=LABEL$group, color.background="lightblue", color.border="darkblue", color.highlight="orangered", size=SmallSize, font.size =FontSize, shape="dot", stringsAsFactors = F)
      edges <- data.frame(from = Graph1$childClassLabel, to = Graph1$subjectLabel, dashes=FALSE)

      Graph2 <- Graph[Graph$property != "wdt:P279",]
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
     }}
  }

  if(any(colnames(Graph) == "parentClassLabel")){
    if(Count > 1){
      for(n in seq_len(length(Count10))){
        if(Count10[n] > Count){
          nodes$color.background[id2 == names(Count10)[n]] <- ColorSpa[n]
          nodes$color.border[id2 == names(Count10)[n]] <- adjustcolor(ColorSpa[n], alpha.f=1/2)
          nodes$size[id2 == names(Count10)[n]] <- Size
          nodes$group[id2 == names(Count10)[n]] <- names(Count10)[n]
        }else{}}
    }else{}
  }

#head(nodes)
#nodes[grepl("owl", nodes$id),]

if(!is.null(ColoredSeed)){
if(grepl("mesh:", nodes$id[1])){
a <- paste0("mesh:", unlist(purrr::map(nodes$id, function(x){strsplit(x, split = "[.]mesh[:]")[[1]][2]})))
}
if(grepl("wd:", nodes$id[1])){
a <- paste0("wd:", unlist(purrr::map(nodes$id, function(x){strsplit(x, split = "[.]wd[:]")[[1]][2]})))
}
if(grepl("obo:", nodes$id[1])){
a <- paste0("obo:", unlist(purrr::map(nodes$id, function(x){strsplit(x, split = "[.]obo[:]")[[1]][2]})))
b <- paste0("obo:", unlist(purrr::map(nodes$id, function(x){strsplit(x, split = "[.]owl[:]")[[1]][2]})))
}

nodes$color.background[a %in% ColoredSeed] <- "#ffff00"
nodes$color.border[a %in% ColoredSeed] <- "#ffd700"
nodes$size[a %in% ColoredSeed] <- 12
nodes$font.color <- "black"
if(!is.null(ColoredTopClass)){
nodes$font.size[a %in% ColoredTopClass] <- 8
nodes$font.color[a %in% ColoredTopClass] <- "red"
}
}

#head(Graph)
if(!any(colnames(Graph) == "propertyLabel")){}else{
    if(any(colnames(Graph) == "parentClassLabel")){}else{
      nodes %>% rbind(nodes2) -> nodes}}
  nodes <- nodes[as.numeric(as.character(rownames(unique(nodes['id'])))),]
  if(is.null(Selected)){
  VIS <-  visNetwork::visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visNetwork::visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>%
      visNetwork::visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visNetwork::visIgraphLayout(layout = Layout,
                                  physics = Physics,
                                  smooth = Smooth,
                                  type = "full",
                                  randomSeed = 1234) %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T), selectedBy = "group", autoResize=T)
  }else{
    VIS <-  visNetwork::visNetwork(nodes, edges, height = HeightSclale, width = WidthSclale) %>%
      visNetwork::visEdges(shadow = F,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "pink")) %>%
      visNetwork::visNodes(borderWidth=0.25, shadow = list(enabled = TRUE, size = 5),
               font=list(size = FontSize)) %>%
      visNetwork::visIgraphLayout(layout = Layout,
                                  physics = Physics,
                                  smooth = Smooth,
                                  type = "full",
                                  randomSeed = 1234) %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=T, selected=Selected), selectedBy = "group", autoResize=T)
  }
  if(outputNodesEdges){
    readr::write_excel_csv(nodes, file = paste("nodes_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") )
    readr::write_excel_csv(edges, file = paste("edges_", format(Sys.time(), "%y%m%d_%H%M"),".csv", sep="") )
  }else{}
  if(Output){
    VIS %>% networkD3::saveNetwork(file = FilePath)
    if(Browse){browseURL(FilePath)}else{return()}
  } else { return(VIS) }

}


