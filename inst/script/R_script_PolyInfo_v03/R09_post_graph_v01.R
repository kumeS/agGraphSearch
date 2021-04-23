rm(list=ls())

library(igraph)
library(magrittr)
library(data.table)
library(graphlayouts)
if(!require("treemap")){install.packages("treemap")}; library(treemap)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)

if(!file.exists("./09_Out")){dir.create("./09_Out")}
QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02_210201.Rdata")
head(QID_rdfsLabel02)
QID_rdfsLabel02$Label.j <- paste0(QID_rdfsLabel02$entityNamej, "(", QID_rdfsLabel02$QID, ")")
QID_rdfsLabel02$Label.e <- paste0(QID_rdfsLabel02$entityNamee, "(", QID_rdfsLabel02$QID, ")")
head(QID_rdfsLabel02)

SearchLabel <- readRDS("./07_Out/SearchNum.Rdata")
head(SearchLabel)
SearchLabel$No <- 1:nrow(SearchLabel)
table(is.na(SearchLabel$entityNamej))
SearchLabel$entityNamej[is.na(SearchLabel$entityNamej)] <- SearchLabel$entityNamee[is.na(SearchLabel$entityNamej)]
SearchLabel$Label.j <- paste0(SearchLabel$entityNamej, "(", SearchLabel$CommonEntity, ")")
SearchLabel$Label.e <- paste0(SearchLabel$entityNamee, "(", SearchLabel$CommonEntity, ")")
head(SearchLabel)

a <- data.frame(readr::read_csv(paste0("./06_Out_txt/Comm.csv"), col_names = F))
head(a)
colnames(a) <- c("Name.QID", "Name", "QID", "CommVal")
b <- merge(SearchLabel, a, by="QID", all = T, sort = F)
head(b)

table(b$CommVal, useNA = "ifany")
b$CommVal[is.na(b$CommVal)] <- 0
table(b$CommVal, useNA = "ifany")

head(b)
b1 <- b[order(b$No),]
head(b1)
rownames(b1) <- 1:nrow(b1)
b1$Name.QID <- b1$Label.j
b1$Name <- b1$entityNamej
head(b1)
apply(b1, 2, function(x) table(is.na(x)))

################################################################################
################################################################################
#上位グラフ
wikiGraph_02 <- readRDS(file = "./03_Out/R03_wikiGraph_02_up_210203.Rdata")
head(wikiGraph_02)
rownames(wikiGraph_02) <- 1:nrow(wikiGraph_02)
head(wikiGraph_02 <- wikiGraph_02[as.numeric(rownames(unique(wikiGraph_02["unique"]))),])
rownames(wikiGraph_02) <- 1:nrow(wikiGraph_02)

dim(wikiGraph_02)
length(unique(wikiGraph_02$unique))
table(is.na(wikiGraph_02$subjectLabel))
table(is.na(wikiGraph_02$parentClassLabel))

wikiGraph_03 <- data.frame(a=paste0(wikiGraph_02$subjectLabel, "(", wikiGraph_02$subject,")"),
                           b=paste0(wikiGraph_02$parentClassLabel, "(", wikiGraph_02$parentClass,")"))
head(wikiGraph_03)

#共通エンティティ・リスト
list <- readRDS("./03_Out/R03_pc_table2_210203.Rdata")
head(list)
list00 <- paste0(list$parentClassLabel, "(", list$parentClass,")")

#起点・リスト
SearchLabel00 <- readRDS("./04_Out/SearchNum00_210204.Rdata")
head(SearchLabel00 <- SearchLabel00[!is.na(SearchLabel00$Levels),])
max(SearchLabel00$Levels)
SearchLabel00$No <- 1:nrow(SearchLabel00)
table(is.na(SearchLabel00))
table(is.na(SearchLabel00$entityNamej))
SearchLabel00$entityNamej[is.na(SearchLabel00$entityNamej)] <- SearchLabel00$entityNamee[is.na(SearchLabel00$entityNamej)]
SearchLabel00$Label.j <- paste0(SearchLabel00$entityNamej, "(", SearchLabel00$CommonEntity, ")")
SearchLabel00$Label.e <- paste0(SearchLabel00$entityNamee, "(", SearchLabel00$CommonEntity, ")")
dim(SearchLabel00)
head(SearchLabel00)
table(SearchLabel00$Levels)
rownames(SearchLabel00) <- 1:nrow(SearchLabel00)
SearchLabel00.cut <- SearchLabel00[SearchLabel00$Levels < 6,]
rownames(SearchLabel00.cut) <- 1:nrow(SearchLabel00.cut)

################################################################################
################################################################################
#Graph
################################################################################
head(wikiGraph_03)
wikiGraph_03[grepl("^エンティティ", wikiGraph_03$b),]

#下位検索
Query <- "エンティティ(wd:Q35120)"
x <- 0
MAX <- 6

wikiGraph_04 <- c()
repeat{
x <- x +1
message(paste0("down: ", x))
c <- c(wikiGraph_03[,2] %in% Query)
b <- wikiGraph_03[c,]
b$c <- x
#head(b)
wikiGraph_04 <- wikiGraph_04 %>% rbind(b)
if(x == MAX){break}
Query <- b$a
}

head(wikiGraph_04)
dim(wikiGraph_04)
table(wikiGraph_04$c)
rownames(wikiGraph_04) <- 1:nrow(wikiGraph_04)

##################################################
#igraph
##################################################
g0 <- simplify(graph_from_edgelist(as.matrix(wikiGraph_03), directed = T))
g <- simplify(graph_from_edgelist(as.matrix(wikiGraph_04[,-3]), directed = T))

wikiGraph_05 <- wikiGraph_04[as.numeric(rownames(unique(wikiGraph_04["a"]))),]
gg <- data.frame(a=names(V(g)), b =NA, c=NA, d=NA, e=NA)

head(gg)
dim(gg)
length(names(V(g)))
head(gg)
head(names(V(g)))
table(gg$a == names(V(g)))

for(m in 1:nrow(gg)){
  #m <- 1
  gg$e[m] <- wikiGraph_05[wikiGraph_05$a %in% gg[m,1], ]$c[1]
}
head(gg)
#table(gg$a == names(V(g)))

for(n in 1:length(names(V(g)))){
#n <- 1
print(paste0("n: ", n, "  ", names(V(g))[n]))
g0s <- subcomponent(g0, c(1:length(names(V(g0))))[names(V(g0)) %in% names(V(g))[n]], "in")
g0s1 <- names(g0s)[!c(names(g0s) %in% names(V(g))[n])]

g0s2 <- length(g0s1[g0s1 %in% QID_rdfsLabel02$Label.j])
gg[n,2] <- g0s2

g0s3 <- length(g0s1[g0s1 %in% unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))])
gg[n,3] <- g0s3

g0s4 <- length(g0s1[g0s1 %in% unique(c(SearchLabel00.cut$Label.j, SearchLabel00.cut$Label.e))])
gg[n,4] <- g0s4
}

head(gg)
#readr::write_excel_csv(gg, file=paste0("./09_Out/gg.csv"), append = F)

#########################
#実行 - 可視化
#########################
#gg <- data.frame(readr::read_csv(gg, file=paste0("./09_Out/gg.csv"), col_names=T))
head(gg)
Int00 <- data.frame(readr::read_csv(paste0("./06_Out_txt/Intermediate_concepts_v01.csv"), col_names = F))$X4
head(Int00)

gg[gg$a == "エンティティ(wd:Q35120)",]$e <- 0
gg_list <- gg[gg$e <= 1,]
SearchEntity <- 175
Kiten01 <- 140
Kiten02 <- 122

for(k in 1:nrow(gg_list)){
#for(k in 14){
#k <- 1
#k <- 2
Graph <- g
Graph00 <- subcomponent(Graph, c(1:length(names(V(Graph))))[names(V(Graph)) %in% gg_list$a[k]], "in")
LIST00 <- unique(c(names(Graph00), "エンティティ(wd:Q35120)"))
#含まれる数
NUM00 <- length(LIST00[LIST00 %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))])
NUM01 <- length(LIST00[LIST00 %in% unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))])
NUM02 <- length(LIST00[LIST00 %in% unique(c(SearchLabel00.cut$Label.j, SearchLabel00.cut$Label.e))])
#tail(list00)
LIST01 <- unique(c(list00))

#サブツリーの下位のみ
V(Graph)$community <- 1
V(Graph)$community[names(V(Graph)) %in% LIST00] <- 2
Graph01 <- induced_subgraph(Graph, V(Graph)$community == 2)
Graph01
#plot(Graph01)

ggg <- data.frame(matrix(NA, nrow=length(names(V(Graph01))), ncol=7))
ggg$X1 <- names(V(Graph01))
for(n in 1:length(names(V(Graph01)))){
#n <- 94
print(paste0("n: ", n))
ggg.s <- subcomponent(Graph01, c(1:length(names(V(Graph01))))[names(V(Graph01)) %in% names(V(Graph01))[n]], "in")
ggg.s1 <- names(ggg.s)[!c(names(ggg.s) %in% names(V(Graph01))[n])]

ggg.s2 <- length(ggg.s1[ggg.s1 %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))])
ggg[n,2] <- round(ggg.s2/NUM00*100, 2)

ggg.s3 <- length(ggg.s1[ggg.s1 %in% unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))])
ggg[n,3] <- round(ggg.s3/NUM01*100, 2)

ggg.s4 <- length(ggg.s1[ggg.s1 %in% unique(c(SearchLabel00.cut$Label.j, SearchLabel00.cut$Label.e))])
ggg[n,4] <- round(ggg.s4/NUM02*100, 2)

ggg[n,5] <- ggg.s2
ggg[n,6] <- ggg.s3
ggg[n,7] <- ggg.s4
}

ggg[is.na(ggg)] <- 0

#共通エンティティ、起点のみ
V(Graph01)$community <- 1
V(Graph01)$community[names(V(Graph01)) %in% LIST01] <- 2
Graph01 <- induced_subgraph(Graph01, V(Graph01)$community == 2)
Graph01

V(Graph01)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(Graph01)) %in% e1
V(Graph01)$color[f1] <- "skyblue"
e2 <- unique(list00)
f2 <- names(V(Graph01)) %in% e2
V(Graph01)$color[f2] <- "lightgreen"
e3 <- unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))
f3 <- names(V(Graph01)) %in% e3
V(Graph01)$color[f3] <- "pink"

(len <- length(V(Graph01)$color))
(len1 <- length(V(Graph01)$color[f1]))
(len2 <- length(V(Graph01)$color[f2]))
(len3 <- length(V(Graph01)$color[f3]))

V(Graph01)$size <- 0.75
V(Graph01)$size[f3] <- 1
E(Graph01)$width <- 0.1
V(Graph01)$label.cex <- 0
CEX=0.025
E(Graph01)$lty <- 1

#gg01
gg01 <- gg[gg$a %in% names(V(Graph01)),]
gg01$f <- gg01$b/SearchEntity*100
rownames(gg01) <- 1:nrow(gg01)

quartz(width=5, height=5)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
layout03 <- as.matrix(data.frame(gg01$e, gg01$f))
el <- data.table::as.data.table(as_edgelist(Graph01)) %>% 
  data.table::setnames(c("x","y"))
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(Graph01)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

dX <- el_w_layout$x2 - el_w_layout$x1
dY <- el_w_layout$y2 - el_w_layout$y1
dX00 <- dX/sqrt(dX^2 + dY^2)*5
dY00 <- dY/sqrt(dX^2 + dY^2)*5

head(d_layout)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
plot(d_layout[,1:2], type = "n",
  ylab = "下位に含まれる検索エンティティ数 per total (%)", 
  xlab = "Wikidataエンティティからの距離(クラス階層)", 
  xlim=c(-0.5, MAX), ylim=c(0, 100))
arrows(x0  = el_w_layout$x1, x = el_w_layout$x2,
         y0  = el_w_layout$y1, y = el_w_layout$y2,
         col = "grey90", length = 0.05)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
if(len < 10000){
ff <- gg01$f > 15
if(any(ff)){
 text(d_layout$x_coord[ff], d_layout$y_coord[ff] - 2, 
  labels=d_layout$id[ff], cex=CEX*5)   
}
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_5.pdf"), type = "pdf"); dev.off()

##サブツリー補正
head(ggg); dim(ggg); colnames(ggg)[1] <- "a"
head(gg01); dim(gg01)
gg01.m <- merge(gg01, ggg, by="a", sort=F, all=F)
#table(gg01.m$a == gg01$a)
#head(gg01.m)

################################################################################################
################################################################################################
###エクセル出力
# All pathways
root <- which(degree(Graph01, v = V(Graph01), mode = "out")==0, useNames = T)
AllPath <- get.all.shortest.paths(Graph01, from = root, to = 1:length(V(Graph01)), mode = "in")
AllPath <- AllPath$res
AllPath00 <- data.frame(matrix(NA, nrow=length(AllPath), ncol=2))
for(n in 1:nrow(AllPath00)){
  # n <- 3
  tes <- names(AllPath[[n]])
  AllPath00[n,1] <- tes[length(tes)]
  AllPath00[n,2] <- paste(tes, collapse ="/")
}

head(AllPath00)
unique(AllPath00$X1)
head(gg01.m)

AllPath01 <- merge(gg01.m, AllPath00, by.x="a", by.y="X1", sort=F, all=F)
head(AllPath01)
colnames(AllPath01)[ncol(AllPath01)] <- "pathString"
AllPath01$SearchEntity <- ""
AllPath01$SearchEntity[c(AllPath01$a %in% e1)] <- "SearchEntity"
AllPath01$CommonEntity <- ""
AllPath01$CommonEntity[c(AllPath01$a %in% e2)] <- "CommonEntity"
AllPath01$ExpandedCommonEntity <- ""
AllPath01$ExpandedCommonEntity[c(AllPath01$a %in% e3)] <- "ExpandedCommonEntity"

AllPath02 <- data.tree::as.Node(AllPath01, na.rm = T)
AllPath03 <- print(AllPath02, "e", "a", "b", "d", "X5", "X7", "X2.x", "X3", 
  "SearchEntity", "CommonEntity", "ExpandedCommonEntity", limit = nrow(AllPath01))
colnames(AllPath03) <- c("levelName", "Distance_from_entity", "Entity_name", 
  "Search", "Kiten", "Subtree_Search", "Subtree_Kiten", "Subtree_Search_percent", "Subtree_Kiten_percent", 
  "SearchEntity", "CommonEntity", "ExpandedCommonEntity")
readr::write_excel_csv(AllPath03, file=paste0("./09_Out/", formatC(k, width=3, flag="0"), "_01_Graph_len_", len, "_", gg_list$a[k], "_tree.csv"), col_names=T)

##########################################
##Suggestions
##########################################
head(AllPath00)
Output <- c()

for(n in 1:nrow(AllPath00)){
# n <- 1
a1 <- strsplit(AllPath00$X2[n], split = "[/]")[[1]]
Res <- c()

for(m in 1:length(a1)){
#m <-1
Res <- Res %>% rbind(gg01.m[gg01.m$a == a1[m],])
}

if(sum(diff(Res$X5) < 0) < 2){
  Output <- c(Output, AllPath00$X2[n])
}}

Output
head(AllPath01)
AllPath01.s <- AllPath01[c(AllPath01$pathString %in% Output), ]
AllPath01.s$CommunityAnalysis <- ifelse(AllPath01.s$a %in% Int00, "CommunityAnalysis", "")

AllPath02.s <- data.tree::as.Node(AllPath01.s, na.rm = T)
AllPath03.s <- print(AllPath02.s, "e", "a", "b", "d", "X5", "X7", "X2.x", "X3", 
  "SearchEntity", "CommonEntity", "ExpandedCommonEntity", "CommunityAnalysis", limit = nrow(AllPath01.s))
colnames(AllPath03.s) <- c("levelName", "Distance_from_entity", "Entity_name", 
  "Search", "Kiten", "Subtree_Search", "Subtree_Kiten", "Subtree_Search_percent", "Subtree_Kiten_percent",
  "SearchEntity", "CommonEntity", "ExpandedCommonEntity", "CommunityAnalysis")
readr::write_excel_csv(AllPath03.s, file=paste0("./09_Out/", formatC(k, width=3, flag="0"), "_02_Graph_len_", len, "_", gg_list$a[k], "_tree_Suggestions.csv"), col_names=T)

################################################################################################
################################################################################################
quartz(width=5, height=5)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
layout03 <- as.matrix(data.frame(gg01.m$e, gg01.m$X2))
el <- data.table::as.data.table(as_edgelist(Graph01)) %>% 
  data.table::setnames(c("x","y"))
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(Graph01)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

dX <- el_w_layout$x2 - el_w_layout$x1
dY <- el_w_layout$y2 - el_w_layout$y1
dX00 <- dX/sqrt(dX^2 + dY^2)*5
dY00 <- dY/sqrt(dX^2 + dY^2)*5

set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
plot(d_layout[,1:2], type = "n",
  ylab = "下位に含まれる検索エンティティ数 per sub-tree (%)", 
  xlab = "Wikidataエンティティからの距離(クラス階層)", 
  xlim=c(-0.5, MAX), ylim=c(0, 100))
arrows(x0  = el_w_layout$x1, x = el_w_layout$x2,
         y0  = el_w_layout$y1, y = el_w_layout$y2,
         col = "grey90", length = 0.05)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
if(len < 10000){
ff <- gg01.m$X2 > 20
if(any(ff)){
 text(d_layout$x_coord[ff], d_layout$y_coord[ff] - 2, 
  labels=d_layout$id[ff], cex=CEX*5)   
}
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_6.pdf"), type = "pdf"); dev.off()

################################################################################################
################################################################################################

quartz(width=5, height=5)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
layout03 <- as.matrix(data.frame(gg01.m$e, gg01.m$X3))
el <- data.table::as.data.table(as_edgelist(Graph01)) %>% 
  data.table::setnames(c("x","y"))
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(Graph01)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

dX <- el_w_layout$x2 - el_w_layout$x1
dY <- el_w_layout$y2 - el_w_layout$y1
dX00 <- dX/sqrt(dX^2 + dY^2)*5
dY00 <- dY/sqrt(dX^2 + dY^2)*5

set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(1, 1, 0.5, 0.5))
plot(d_layout[,1:2], type = "n",
  ylab = "下位に含まれる起点数 per sub-tree (%)", 
  xlab = "Wikidataエンティティからの距離(クラス階層)", 
  xlim=c(-0.5, MAX), ylim=c(0, 100))
arrows(x0  = el_w_layout$x1, x = el_w_layout$x2,
         y0  = el_w_layout$y1, y = el_w_layout$y2,
         col = "grey90", length = 0.05)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
if(len < 10000){
ff <- gg01.m$X3 > 20
if(any(ff)){
 text(d_layout$x_coord[ff], d_layout$y_coord[ff] - 2, 
  labels=d_layout$id[ff], cex=CEX*5)   
}
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_7.pdf"), type = "pdf"); dev.off()

##plot
#system.time(layout03 <- layout_with_fr(Graph01))
system.time(layout03 <- layout_with_graphopt(Graph01, niter = 10000))
#system.time(layout03 <- layout_as_tree(Graph01))
layout03 <- layout03 - range(layout03)[1]
el <- data.table::as.data.table(as_edgelist(Graph01)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(Graph01)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

dX <- el_w_layout$x2 - el_w_layout$x1
dY <- el_w_layout$y2 - el_w_layout$y1
dX00 <- dX/sqrt(dX^2 + dY^2)*5
dY00 <- dY/sqrt(dX^2 + dY^2)*5

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1+ dX00, x = el_w_layout$x2 - dX00,
         y0  = el_w_layout$y1+ dY00, y = el_w_layout$y2 - dY00,
         col = "grey90", length = 0.025)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_1.pdf"), type = "pdf"); dev.off()

V(Graph01)$size <- c(gg01$b/SearchEntity*3)
head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1 + dX00, x = el_w_layout$x2 - dX00,
         y0  = el_w_layout$y1 + dY00, y = el_w_layout$y2 - dY00,
         col = "grey90", length = 0.025)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
legend("topleft", legend=c(1, 10, 50, 100, 150), y.intersp = 1.3,
  pch=21, pt.cex=c(1, 10, 50, 100, 150)*lm(V(Graph01)$size ~ gg01$b)$coefficients[2])
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, 
  labels=paste0(d_layout$id, "\nWikidataエンティティからの深さ: ", gg01$e, "\n下位の検索エンティティ数: ", gg01$b, "\n下位の起点数: ", gg01$c), cex=CEX)  
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_2.pdf"), type = "pdf"); dev.off()

V(Graph01)$size <- gg01$d/Kiten02*3
head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1 + dX00, x = el_w_layout$x2 - dX00,
         y0  = el_w_layout$y1 + dY00, y = el_w_layout$y2 - dY00,
         col = "grey90", length = 0.025)
legend("topleft", legend=c(1, 10, 50, 100, 150), y.intersp = 1.3,
  pch=21, pt.cex=c(1, 10, 50, 100, 150)*lm(V(Graph01)$size ~ gg01$c)$coefficients[2])
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, 
  labels=paste0(d_layout$id, "\nWikidataエンティティからの深さ: ", gg01$e, "\n下位の検索エンティティ数: ", gg01$b, "\n下位の起点数: ", gg01$c), cex=CEX)  
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_3.pdf"), type = "pdf"); dev.off()

V(Graph01)$size <- gg01$b/Kiten01*3
V(Graph01)$color <- "lightgreen"
V(Graph01)$color[gg01$e == 1] <- "pink"
V(Graph01)$color[gg01$e == 2] <- "skyblue"
V(Graph01)$color[gg01$e == 3] <- "lightyellow"
V(Graph01)$color[gg01$e == 4] <- "lightslateblue"

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1 + dX00, x = el_w_layout$x2 - dX00,
         y0  = el_w_layout$y1 + dY00, y = el_w_layout$y2 - dY00,
         col = "grey90", length = 0.025)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(Graph01)$size, bg=V(Graph01)$color)
legend("topleft", legend=c(1, 10, 50, 100, 150), y.intersp = 1.3,
  pch=21, pt.cex=c(1, 10, 50, 100, 150)*lm(V(Graph01)$size ~ gg01$b)$coefficients[2])
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, 
  labels=paste0(d_layout$id, "\nWikidataエンティティからの深さ: ", gg01$e, "\n下位の検索エンティティ数: ", gg01$b, "\n下位の起点数: ", gg01$c), cex=CEX)  
}
quartz.save(file = paste0("./09_Out/", formatC(k, width=3, flag="0"), "_Graph_len_", len, "_4.pdf"), type = "pdf"); dev.off()

}



################################################################################
##出力ファイルの結合
################################################################################
abcd00 <- dir("./09_Out", pattern = "_tree_Suggestions.csv")
for(k in 1:nrow(gg_list)){
#k <- 1
if(k == 1){
readr::write_excel_csv(data.frame(k),
                       file=paste0("./09_Out/ALL_tree_Suggestions.csv"), 
                       append=F, col_names=F)
system(paste0("cat './09_Out/", abcd00[k] , "' >> './09_Out/ALL_tree_Suggestions.csv'"))
readr::write_excel_csv(data.frame(""),
                       file=paste0("./09_Out/ALL_tree_Suggestions.csv"), 
                       append=T, col_names=F)
}else{
readr::write_excel_csv(data.frame(k),
                       file=paste0("./09_Out/ALL_tree_Suggestions.csv"), 
                       append=T, col_names=F)
system(paste0("cat './09_Out/", abcd00[k] , "' >> './09_Out/ALL_tree_Suggestions.csv'"))
readr::write_excel_csv(data.frame(""),
                       file=paste0("./09_Out/ALL_tree_Suggestions.csv"), 
                       append=T, col_names=F)
}
}



