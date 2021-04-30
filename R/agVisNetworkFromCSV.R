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
