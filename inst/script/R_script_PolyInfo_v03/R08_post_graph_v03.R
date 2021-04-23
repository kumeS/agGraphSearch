################################################################################
################################################################################
rm(list=ls())

library(igraph)
library(magrittr)
library(data.table)
library(graphlayouts)
if(!require("treemap")){install.packages("treemap")}; library(treemap)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)

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

list <- readRDS("./03_Out/R03_pc_table2_210203.Rdata")
head(list)
list00 <- paste0(list$parentClassLabel, "(", list$parentClass,")")

SearchLabel00 <- readRDS("./04_Out/SearchNum00_210204.Rdata")
head(SearchLabel00 <- SearchLabel00[!is.na(SearchLabel00$Levels),])
max(SearchLabel00$Levels)
SearchLabel00$No <- 1:nrow(SearchLabel00)
table(is.na(SearchLabel00))
table(is.na(SearchLabel00$entityNamej))
SearchLabel00$entityNamej[is.na(SearchLabel00$entityNamej)] <- SearchLabel00$entityNamee[is.na(SearchLabel00$entityNamej)]
SearchLabel00$Label.j <- paste0(SearchLabel00$entityNamej, "(", SearchLabel00$CommonEntity, ")")
SearchLabel00$Label.e <- paste0(SearchLabel00$entityNamee, "(", SearchLabel00$CommonEntity, ")")
head(SearchLabel00)

################################################################################
#Graph
################################################################################
g <- simplify(graph_from_edgelist(as.matrix(wikiGraph_03), directed = T))

V(g)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(g)) %in% e1
V(g)$color[f1] <- "skyblue"
e2 <- unique(list00)
f2 <- names(V(g)) %in% e2
V(g)$color[f2] <- "lightgreen"
e3 <- unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))
f3 <- names(V(g)) %in% e3
V(g)$color[f3] <- "pink"

(len <- length(V(g)$color))
(len1 <- length(V(g)$color[f1]))
(len2 <- length(V(g)$color[f2]))
(len3 <- length(V(g)$color[f3]))

V(g)$size <- 0.75
V(g)$size[f3] <- 1
E(g)$width <- 0.1
V(g)$label.cex <- 0
CEX=0.025
E(g)$lty <- 1

##plot
#system.time(layout03 <- layout_with_fr(g))
system.time(layout03 <- layout_with_graphopt(g, niter = 5000))
layout03 <- layout03 - range(layout03)[1]
el <- data.table::as.data.table(as_edgelist(g)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(g)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1, x   = el_w_layout$x2 - (el_w_layout$x2 -  el_w_layout$x1)*0.03,
         y0  = el_w_layout$y1, y   = el_w_layout$y2 - (el_w_layout$y2 -  el_w_layout$y1)*0.03,
         col = "grey90", length = 0.02)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color)
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
quartz.save(file = paste0("./08_Out/up_Graph_len_", len, ".pdf"), type = "pdf"); dev.off()

################################################################################
#SubGraph
################################################################################
list01 <- names(neighbors(g, v=c(1:length(names(V(g))))[names(V(g)) %in% "エンティティ(wd:Q35120)"], mode = "in"))

for(k in 1:length(list01)){
#k <- 1
abc <- c(1:length(names(V(g))))[names(V(g)) %in% list01[k]]
g1 <- subcomponent(g, v=abc, mode = "in")

V(g)$community <- 1
V(g)$community[names(V(g)) %in% names(g1)] <- 2
g2 <- induced_subgraph(g, V(g)$community == 2)

V(g2)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(g2)) %in% e1
V(g2)$color[f1] <- "skyblue"
e2 <- unique(list00)
f2 <- names(V(g2)) %in% e2
V(g2)$color[f2] <- "lightgreen"
e3 <- unique(c(SearchLabel00$Label.j, SearchLabel00$Label.e))
f3 <- names(V(g2)) %in% e3
V(g2)$color[f3] <- "pink"

(len <- length(V(g2)$color))
(len1 <- length(V(g2)$color[f1]))
(len2 <- length(V(g2)$color[f2]))
(len3 <- length(V(g2)$color[f3]))

V(g2)$size <- 0.75
V(g2)$size[f3] <- 1
E(g2)$width <- 0.1
V(g2)$label.cex <- 0
CEX=0.025
E(g2)$lty <- 1

##plot
#system.time(layout03 <- layout_with_fr(g2))
system.time(layout03 <- layout_with_graphopt(g2, niter = 5000))
layout03 <- layout03 - range(layout03)[1]
el <- data.table::as.data.table(as_edgelist(g2)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(g2)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
arrows(x0  = el_w_layout$x1, x   = el_w_layout$x2 - (el_w_layout$x2 -  el_w_layout$x1)*0.05,
         y0  = el_w_layout$y1, y   = el_w_layout$y2 - (el_w_layout$y2 -  el_w_layout$y1)*0.05,
         col = "grey90", length = 0.035)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(g2)$size, bg=V(g2)$color)
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
quartz.save(file = paste0("./08_Out/up_Graph_", formatC(k, width=4, flag="0"), "_len_", len, ".pdf"), type = "pdf"); dev.off()
}

################################################################################
#OCR
#日本語セッション
OCR_jpn <- readr::read_csv("./高分子辞典OCR/高分子辞典OCR_2column_jpn_RRR.csv", col_names=F)
#英語セッション
OCR_en <- readr::read_csv("./高分子辞典OCR/高分子辞典OCR_2column_en.csv", col_names=F)
#日本語+英語セッション
OCR_all <- readr::read_csv("./高分子辞典OCR/高分子辞典OCR_2column_All_RR.csv", col_names=F)

#前処理、Unique
dim(OCR_jpn)
length(as.character(OCR_jpn$X1))
head(OCR_jpn <- unique(as.character(OCR_jpn$X1)))
head(OCR_en <- unique(as.character(OCR_en$X1)))
head(OCR_all <- unique(as.character(OCR_all$X1)))

################################################################################
#個別グラフ: 全体を可視化する
for(n in 1:nrow(b1)){
#for(n in 1:65){
#n <- 1
#n <- 35
#n <- 44  
#n <- 67
#n <- 68
#n <- 78
#head(b1)
print(n)

d1 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], ".csv"), col_names = F))  
d2 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_label.csv"), col_names = F))  
head(d2)
d2 <- d2[d2$X4 == "rdfs:label",]
d2 <- d2[order(d2$X3, decreasing = T),]
d2$X5 <- paste0(d2$X2, "(", d2$X1, ")")
rownames(d2) <- 1:nrow(d2)
d2 <- d2[as.numeric(rownames(unique(d2["X1"]))),]
d2 <- d2[,c(1,5)]
d2$X3 <- d2$X1
colnames(d2) <- c("X1", "X2", "X3")

############################
head(d1)
head(d2)
d3 <- merge(d1, d2, by="X1", all=F, sor=F)
#head(d3)
d3 <- d3[,c(1:9)]
colnames(d3) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
############################
d4 <- merge(d3, d2, by="X3", all=F, sor=F)
#head(d4)
d4 <- d4[,c(2:3, 1, 4:9, 11)]
colnames(d4) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")

#apply(d3, 2, function(x) table(is.na(x), useNA="ifany"))
#d3[is.na(d3$X2),]
#d3[d3$X1 == "wd:Q35120",]
#d4[is.na(d4$X2),]
#d4[d3$X1 == "wd:Q35120",]
#d4 <- d4[!is.na(d4$X2),]

d4.down <- d4[d4$X4 == "down",]
d4.down.s <- d4.down[d4.down$X2 == "wdt:P279",]
d4.down.i <- d4.down[d4.down$X2 == "wdt:P31",]

#head(d3)
head(d4)
head(d4.down)
head(d4.down.s)
head(d4.down.i)
############################
#All
g <- simplify(graph_from_edgelist(as.matrix(d4.down[,c(9:10)]), directed = T))

V(g)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(g)) %in% e1
V(g)$color[f1] <- "skyblue"
e2 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))
f2 <- names(V(g)) %in% e2
V(g)$color[f2] <- "pink"
e3 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))[unique(c(SearchLabel$Label.j, SearchLabel$Label.e)) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))]
f3 <- names(V(g)) %in% e3
V(g)$color[f3] <- "lightgreen"

len <- length(V(g)$color)
len1 <- length(V(g)$color[f1])
len2 <- length(V(g)$color[f2])
len3 <- length(V(g)$color[f3])

if(length(names(V(g))) < 100){
V(g)$size <- 3
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.75
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.5
V(g)$label.cex <- 0.1
CEX=0.1
}else{
if(length(names(V(g))) < 500){
V(g)$size <- 2
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.5
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.4
V(g)$label.cex <- 0.05
CEX=0.05
}else{
V(g)$size <- 0.5
V(g)$size[f1] <- 1.5
V(g)$size[f2] <- 1.5
E(g)$width <- 0.1
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.1
V(g)$label.cex <- 0
CEX=0.01
}}

E(g)$lty <- 1
E(g)$lty[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 3

##plot
system.time(layout03 <- layout_with_fr(g))
el <- data.table::as.data.table(as_edgelist(g)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(g)))

el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
segments(x0  = el_w_layout$x1, x   = el_w_layout$x2,
         y0  = el_w_layout$y1, y   = el_w_layout$y2,
         col = scales::alpha("grey", .3))
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color,
       main=paste0( b1$Label.j[n], "\nTotal: ", len, ", Search: ", len1, ", Common: ", len2, ", both: ", len3))
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
#quartz.save(file = paste("./08_Out/Graph_d4.down_01_", formatC(n, width=4, flag="0"), "_len_", len, ".png", sep=""), type = "png", dpi=300); dev.off()
quartz.save(file = paste("./08_Out/Graph_d4.down_01_", formatC(n, width=4, flag="0"), "_len_", len, ".pdf", sep=""), type = "pdf"); dev.off()
}

################################################################################
################################################################################
################################################################################
################################################################################
#個別グラフ: サブツリー解析
for(n in 1:nrow(b1)){
#for(n in 1:65){
#n <- 1
#n <- 35
#n <- 44
#n <- 67
#n <- 68
#n <- 78
#head(b1)
print(n)

d1 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], ".csv"), col_names = F))  
d2 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_label.csv"), col_names = F))  
head(d2)
d2 <- d2[d2$X4 == "rdfs:label",]
d2 <- d2[order(d2$X3, decreasing = T),]
d2$X5 <- paste0(d2$X2, "(", d2$X1, ")")
rownames(d2) <- 1:nrow(d2)
d2 <- d2[as.numeric(rownames(unique(d2["X1"]))),]
d2 <- d2[,c(1,5)]
d2$X3 <- d2$X1
colnames(d2) <- c("X1", "X2", "X3")

########################################################
head(d1)
head(d2)
d3 <- merge(d1, d2, by="X1", all=F, sor=F)
#head(d3)
d3 <- d3[,c(1:9)]
colnames(d3) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
########################################################
d4 <- merge(d3, d2, by="X3", all=F, sor=F)
#head(d4)
d4 <- d4[,c(2:3, 1, 4:9, 11)]
colnames(d4) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")

#apply(d3, 2, function(x) table(is.na(x), useNA="ifany"))
#d3[is.na(d3$X2),]
#d3[d3$X1 == "wd:Q35120",]
#d4[is.na(d4$X2),]
#d4[d3$X1 == "wd:Q35120",]
#d4 <- d4[!is.na(d4$X2),]

d4.down <- d4[d4$X4 == "down",]
d4.down.s <- d4.down[d4.down$X2 == "wdt:P279",]
d4.down.i <- d4.down[d4.down$X2 == "wdt:P31",]

#head(d3)
head(d4)
head(d4.down)
head(d4.down.s)
head(d4.down.i)
############################
#All
g <- simplify(graph_from_edgelist(as.matrix(d4.down[,c(9:10)]), directed = T))

if(b1$Levels[n] == 1){
#head(b1); n <- 1
abc <- data.frame(a=b1$CommonEntity[n], b=b1$Label.j[n], c=NA, d=NA, e=NA, f=NA)
list000 <- names(V(g))
list001 <- unique(as.character(sapply(list000, function(x){strsplit(x, "[(]wd[:]")[[1]][1]})))

abc$c <- length(list001)
abc$d <- sum(list001 %in% OCR_jpn)

head(QID_rdfsLabel02)
abc$e <- sum(list000 %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e)))

abc$f <- ifelse(all(c(abc$d > 0, abc$e > 0)), "OCR_jpn_SearchEntity_hits", "")

colnames(abc) <- c("QID", "Label.j", "Label Number", "OCR_jpn_hits", "SearchEntity Number", "hits")
readr::write_excel_csv(abc,
                       file=paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_subtree_OCR_jpn_hits.csv"), 
                       append=F, col_names=T)
}else{
Nei000 <- names(neighbors(g, v=c(1:length(names(V(g))))[names(V(g)) %in% b1$Label.j[n]], mode = "in"))
Nei001 <- paste0(b1$Label.j[n], "/", Nei000)
list001 <- unique(as.character(sapply(Nei000, function(x){strsplit(x, "[(]wd[:]")[[1]][1]})))
list002 <- unique(as.character(sapply(Nei000, function(x){strsplit(x, "[(]wd[:]|[)]$")[[1]][2]})))

abc <- data.frame(pathString=Nei001, a=paste0("wd:", list002), b=Nei000, c=NA, d=NA, e=NA, f=NA)

for(m in 1:length(Nei000)){
#m <- 1
Nei002 <- subcomponent(g, c(1:length(names(V(g))))[names(V(g)) %in% Nei000[m]], "in")
list001 <- unique(as.character(sapply(names(Nei002), function(x){strsplit(x, "[(]wd[:]")[[1]][1]})))
abc$c[m] <- length(list001)
abc$d[m] <- sum(list001 %in% OCR_jpn)
abc$e[m] <- sum(names(Nei002) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e)))
abc$f[m] <- ifelse(all(c(abc$d[m] > 0, abc$e[m] > 0)), "OCR_jpn_SearchEntity_hits", "")

abc$d[m] <- ifelse(abc$d[m] == 0, "", abc$d[m])
abc$e[m] <- ifelse(abc$e[m] == 0, "", abc$e[m])
}

abc00 <- abc[abc$c != 1,]

abc01 <- data.frame(pathString=b1$Label.j[n], a=b1$CommonEntity[n], b=b1$Label.j[n], c=NA, d=NA, e=NA, f=NA)
abc01$c <- length(names(V(g)))
list001g <- unique(as.character(sapply(names(V(g)), function(x){strsplit(x, "[(]wd[:]")[[1]][1]})))
abc01$d <- sum(list001g %in% OCR_jpn)
abc01$e <- sum(names(V(g)) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e)))
abc01$f <- ifelse(all(c(abc01$d > 0, abc01$e > 0)), "OCR_jpn_SearchEntity_hits", "")

#names(V(g))[names(V(g)) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))]

abc02 <- rbind(abc01, abc00)
rownames(abc02) <- 1:nrow(abc02)
abc03 <- data.tree::as.Node(abc02, na.rm = T)

abc04 <- print(abc03, "a", "b", "c", "d", "e", "f", limit = nrow(abc03))
colnames(abc04) <- c("levelName", "QID", "Label.j", "Label Number", "OCR_jpn_hits", "SearchEntity Number", "hits")

readr::write_excel_csv(abc04,
                       file=paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_subtree_OCR_jpn_hits.csv"), 
                       append=F, col_names=T)
}}

################################################################################
##出力ファイルの結合
################################################################################
for(n in 1:nrow(b1)){
#n <- 40
if(n == 1){
readr::write_excel_csv(data.frame(n, b1$Name.QID[n]),
                       file=paste0("./08_Out/ALL_subtree_OCR_jpn_hits.csv"), 
                       append=F, col_names=F)
system(paste0("cat '", paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_subtree_OCR_jpn_hits.csv"), "' >> './08_Out/ALL_subtree_OCR_jpn_hits.csv'"))
readr::write_excel_csv(data.frame(""),
                       file=paste0("./08_Out/ALL_subtree_OCR_jpn_hits.csv"), 
                       append=T, col_names=F)
}else{
readr::write_excel_csv(data.frame(n, b1$Name.QID[n]),
                       file=paste0("./08_Out/ALL_subtree_OCR_jpn_hits.csv"), 
                       append=T, col_names=F)
system(paste0("cat '", paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_subtree_OCR_jpn_hits.csv"), "' >> './08_Out/ALL_subtree_OCR_jpn_hits.csv'"))
readr::write_excel_csv(data.frame(""),
                       file=paste0("./08_Out/ALL_subtree_OCR_jpn_hits.csv"), 
                       append=T, col_names=F)
}}

#system("open ./08_Out/ALL_subtree_OCR_jpn_hits.csv")
################################################################################
################################################################################
#個別グラフ: 検索エンティティの方向を取得する
for(n in 1:nrow(b1)){
#for(n in 1:65){
#n <- 1
#n <- 39
#n <- 67
#n <- 68
#n <- 78
#head(b1)
print(n)

d1 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_uni.csv"), col_names = F))  
d2 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_label.csv"), col_names = F))  
head(d1)
head(d2)

d2 <- d2[d2$X4 == "rdfs:label",]
d2 <- d2[order(d2$X3, decreasing = T),]
d2$X5 <- paste0(d2$X2, "(", d2$X1, ")")
rownames(d2) <- 1:nrow(d2)
d2 <- d2[as.numeric(rownames(unique(d2["X1"]))),]
d2 <- d2[,c(1,5)]
d2$X3 <- d2$X1
colnames(d2) <- c("X1", "X2", "X3")

########################################################
head(d1)
head(d2)
d3 <- merge(d1, d2, by="X1", all=F, sor=F)
#head(d3)
d3 <- d3[,c(1:9)]
colnames(d3) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
########################################################
d4 <- merge(d3, d2, by="X3", all=F, sor=F)
#head(d4)
d4 <- d4[,c(2:3, 1, 4:9, 11)]
colnames(d4) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")

#apply(d3, 2, function(x) table(is.na(x), useNA="ifany"))
#d3[is.na(d3$X2),]
#d3[d3$X1 == "wd:Q35120",]
#d4[is.na(d4$X2),]
#d4[d3$X1 == "wd:Q35120",]
#d4 <- d4[!is.na(d4$X2),]
#head(d4)

d4.down <- d4[d4$X4 == "down",]
d4.down.s <- d4.down[d4.down$X2 == "wdt:P279",]
d4.down.i <- d4.down[d4.down$X2 == "wdt:P31",]

#head(d3)
head(d4)
head(d4.down)
head(d4.down.s)
head(d4.down.i)
########################################################
#All
########################################################
g <- simplify(graph_from_edgelist(as.matrix(d4.down[,c(9:10)]), directed = T))

V(g)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(g)) %in% e1
V(g)$color[f1] <- "skyblue"
e2 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))
f2 <- names(V(g)) %in% e2
V(g)$color[f2] <- "pink"
e3 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))[unique(c(SearchLabel$Label.j, SearchLabel$Label.e)) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))]
f3 <- names(V(g)) %in% e3
V(g)$color[f3] <- "lightgreen"

len <- length(V(g)$color)
len1 <- length(V(g)$color[f1])
len2 <- length(V(g)$color[f2])
len3 <- length(V(g)$color[f3])

if(length(names(V(g))) < 100){
V(g)$size <- 3
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.75
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.5
V(g)$label.cex <- 0.1
CEX=0.1
}else{
if(length(names(V(g))) < 500){
V(g)$size <- 2
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.5
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.4
V(g)$label.cex <- 0.05
CEX=0.05
}else{
V(g)$size <- 0.5
V(g)$size[f1] <- 1.5
V(g)$size[f2] <- 1.5
E(g)$width <- 0.1
E(g)$width[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 0.1
V(g)$label.cex <- 0
CEX=0.01
}}

E(g)$lty <- 1
E(g)$lty[attr(E(g), "vnames") %in% paste0(d4.down.i$X9, "|", d4.down.i$X10)] <- 3

##########################################
#Pathway
##########################################
h1 <- c(1:length(names(V(g))))[names(V(g)) %in% b1$Name.QID[n]]
h2 <- c(1:length(names(V(g))))[names(V(g)) %in% QID_rdfsLabel02$Label.j]
h2 <- h2[!c(h2 %in% h1)]
i <- c()
for(m in 1:length(h2)){
#m <- 1
suppressMessages(h3 <- names(get.shortest.paths(g, from=h1, to=h2[m], mode = "in")$vpath[[1]]))
if(length(h3) == 2){
i[[m]] <- NA
}else{
i[[m]] <- c(h3)  
}}

if(all(is.na(unlist(i)))){
V(g)$color <- "firebrick2"
readr::write_excel_csv(d4,
                       file=paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_uni_cut.csv"), 
                       append=F, col_names=F)
}else{
ii <- c()
for(l in 1:length(i)){
#l <- 1
if(!all(is.na(unlist(i[[l]])))){
#l <- 2
i0 <- i[[l]]
i0 <- i0[!c(i0 %in% b1$Name.QID[n])]
for(k in 1:length(i0)){
#k <- 1
ii <- c(ii, names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) %in% i0[k]], "in")))
}
}else{
ii <- c(ii, names(neighbors(g, v=b1$Name.QID[n], mode = "in")))
}
}
V(g)$color[names(V(g)) %in% unique(ii)] <- "firebrick2"

#head(d4)
head(d5 <- d4)
d5.up <- d5[d5$X4 == "up",]
d5.down <- d5[d5$X4 == "down",]
head(d5.down)
d5.down$X11 <- d5.down$X9 %in% c(ii, b1$Name.QID[n])
d5.down$X12 <- d5.down$X10 %in% c(ii, b1$Name.QID[n])
d5.down2 <- d5.down[unlist(apply(d5.down[,11:12], 1, all)),c(1:10)]
d6 <- rbind(d5.up, d5.down2)
#head(d6)
readr::write_excel_csv(d6,
                       file=paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_uni_cut.csv"), 
                       append=F, col_names=F)
}

##plot
system.time(layout03 <- layout_with_fr(g))
el <- data.table::as.data.table(as_edgelist(g)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1], y_coord = layout03[,2], id = names(V(g)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
segments(x0  = el_w_layout$x1, x   = el_w_layout$x2,
         y0  = el_w_layout$y1, y   = el_w_layout$y2,
         col = scales::alpha("grey", .3))
points(d_layout$x_coord, d_layout$y_coord, pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color,
       main=paste0( b1$Label.j[n], "\nTotal: ", len, ", Search: ", len1, ", Common: ", len2, ", both: ", len3))
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
quartz.save(file = paste("./08_Out/Graph_d4.down_02_", formatC(n, width=4, flag="0"), "_len_", len, ".png", sep=""), type = "png", dpi=300); dev.off()
#quartz.save(file = paste("./08_Out/Graph_d4.down_02_", formatC(n, width=4, flag="0"), "_len_", len, ".pdf", sep=""), type = "pdf"); dev.off()

##################################
#New
##################################
d7 <- data.frame(readr::read_csv(paste0("./08_Out/", formatC(n, width=5, flag="0"), "_", b1$CommonEntity[n], "_uni_cut.csv"), col_names = F))  
#All
head(d7)
d7.down <- d7[d7$X4 == "down",]
d7.down.s <- d7.down[d7.down$X2 == "wdt:P279",]
d7.down.i <- d7.down[d7.down$X2 == "wdt:P31",]

g <- simplify(graph_from_edgelist(as.matrix(d7.down[,c(9:10)]), directed = T))

V(g)$color <- "lightyellow"
e1 <- unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))
f1 <- names(V(g)) %in% e1
V(g)$color[f1] <- "skyblue"
e2 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))
f2 <- names(V(g)) %in% e2
V(g)$color[f2] <- "pink"
e3 <- unique(c(SearchLabel$Label.j, SearchLabel$Label.e))[unique(c(SearchLabel$Label.j, SearchLabel$Label.e)) %in% unique(c(QID_rdfsLabel02$Label.j, QID_rdfsLabel02$Label.e))]
f3 <- names(V(g)) %in% e3
V(g)$color[f3] <- "lightgreen"

len <- length(V(g)$color)
len1 <- length(V(g)$color[f1])
len2 <- length(V(g)$color[f2])
len3 <- length(V(g)$color[f3])

if(length(names(V(g))) < 100){
V(g)$size <- 5
V(g)$size[f1] <- 8
V(g)$size[f2] <- 8
E(g)$width <- 0.75
E(g)$width[attr(E(g), "vnames") %in% paste0(d7.down.i$X9, "|", d7.down.i$X10)] <- 0.5
V(g)$label.cex <- 0.1
CEX=0.1
}else{
if(length(names(V(g))) < 500){
V(g)$size <- 2
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.5
E(g)$width[attr(E(g), "vnames") %in% paste0(d7.down.i$X9, "|", d7.down.i$X10)] <- 0.4
V(g)$label.cex <- 0.05
CEX=0.05
}else{
V(g)$size <- 0.5
V(g)$size[f1] <- 1.5
V(g)$size[f2] <- 1.5
E(g)$width <- 0.1
E(g)$width[attr(E(g), "vnames") %in% paste0(d7.down.i$X9, "|", d7.down.i$X10)] <- 0.1
V(g)$label.cex <- 0
CEX=0.01
}}

E(g)$lty <- 1
E(g)$lty[attr(E(g), "vnames") %in% paste0(d7.down.i$X9, "|", d7.down.i$X10)] <- 3

##plot
system.time(layout03 <- layout_with_fr(g))
el <- data.table::as.data.table(as_edgelist(g)) %>% 
  data.table::setnames(c("x","y"))
    
d_layout <- data.table::data.table(x_coord = layout03[,1],
                       y_coord = layout03[,2], id = names(V(g)))

el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
quartz(width=7.5, height=7.5)
set.seed(123)
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA, 
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
segments(x0  = el_w_layout$x1, x   = el_w_layout$x2,
         y0  = el_w_layout$y1, y   = el_w_layout$y2,
         col = scales::alpha("grey", .3))
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color,
       main=paste0( b1$Label.j[n], "\nTotal: ", len, ", Search: ", len1, ", Common: ", len2, ", both: ", len3))
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)  
}
quartz.save(file = paste("./08_Out/Graph_d4.down_03_", formatC(n, width=4, flag="0"), "_len_", len, ".png", sep=""), type = "png", dpi=300); dev.off()
}






