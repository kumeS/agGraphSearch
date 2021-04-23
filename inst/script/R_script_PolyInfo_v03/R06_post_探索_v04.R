#ファイル全体
rm(list=ls())
Folder_out <- "06_Out_txt"
if(!file.exists(Folder_out)){dir.create(Folder_out)}

#####################################################################
#####################################################################
#起点包含のネットワーク可視化
#####################################################################
Folder_in <- "05_Out_txt"
list <- data.frame(readr::read_csv(paste0("./", Folder_in, "/list.csv"), col_names = F))

head(list)
EdgeList <- c()
EdgeList00 <- c()

for(n in 1:nrow(list)){
#n <- 1
if(!is.na(list$X10[n])){
print(n)
list01 <- unlist(strsplit(list$X10[n], "; "))
a <- data.frame(from=list01, to=list$X9[n])
EdgeList　<- rbind(EdgeList, a)
}else{
print(n)
b <- data.frame(from=list$X9[n], to=list$X9[n])
EdgeList00　<- rbind(EdgeList00, b)
}}

head(EdgeList)
head(EdgeList00)
length(unique(paste0(EdgeList$from, EdgeList$to, sep=".")))

library(igraph)
EdgeList01 <- rbind(EdgeList, EdgeList00)
length(unique(paste0(EdgeList01$from, EdgeList01$to, sep=".")))
D00 <- simplify(graph_from_edgelist(as.matrix(EdgeList01), directed = T))

str(EdgeList)

#可視化
quartz(width=7.5, height=7.5)
set.seed(123)
plot(D00,
     layout=layout_with_graphopt(D00, niter = 5000),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = "skyblue",
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_01.pdf", sep=""), type = "pdf"); dev.off()
###########################################################################################
###########################################################################################
###########################################################################################
#ループ検出
if(F){
g <- simplify(graph_from_edgelist(as.matrix(EdgeList), directed = T))
g

len <- 1:122
d <- data.frame(matrix(NA, nrow=length(len), ncol=length(len)))

for(n in len){
for(m in len){
#n <- 1; m <- 3
print(n)
if(n != m){
try(abc <- shortest_paths(g, 
  from=V(g)[n],
  to = V(g)[m], 
  mode = "out",
  output = "both")$epath[[1]], silent=T)
#str(abc)
if(!identical(attr(abc, "vnames"), character(0))){
d[m,n] <- paste0(attr(abc, "vnames"), collapse = ";")
}else{
d[m,n] <- "0"  
}}}}

head(d)
d.low <- unlist(d[lower.tri(d)])
d.up <- unlist(t(d)[lower.tri(d)])

head(d.low)
head(d.up)
d[1:2,1:2]
t(d[1:2,1:2])

X00 <- data.frame(X=d.up, Y=d.low)
head(X00)

X00 <- X00[c(X00$X != "0"),]
X00 <- X00[X00$Y != "0",]
X00
#table(X00$X == "0")
#table(X00$Y == "0")
readr::write_excel_csv(X00, paste0(Folder_out, "/X00.csv"))

#
Dat <- c()
List <- c("00101_wd:Q7184903.csv", "00099_wd:Q16889133.csv")
for(n in 1:2){
#n <- 1
Dat <- Dat %>% rbind(data.frame(readr::read_csv(paste0("./05_Out_txt/", List[n]), col_names = F)))
}
head(Dat)
Dat <- Dat[Dat$X4 == "down",]
g00 <- simplify(graph_from_edgelist(as.matrix(Dat[,c(1,3)]), directed = T))

table(c(Dat$X1, Dat$X3) ==  "wd:Q7184903")
table(c(Dat$X1, Dat$X3) ==  "wd:Q16889133")

try(abc1 <- shortest_paths(g00, 
  from="wd:Q7184903",
  to = "wd:Q16889133", 
  mode = "out",
  output = "both"), silent=T)
try(abc2 <- shortest_paths(g00, 
  from="wd:Q16889133",
  to = "wd:Q7184903", 
  mode = "out",
  output = "both"), silent=T)

a <- as.character(c(attr(abc1$epath[[1]], "vnames"), attr(abc2$epath[[1]], "vnames")))
b <- matrix(strsplit(paste(sub("\\|", " ", a), collapse = " "), " ")[[1]], ncol=2, byrow=T)
plot(graph_from_edgelist(b, directed = T))
}
###########################################################################################
###########################################################################################
###########################################################################################
#ループ除外
#head(EdgeList)
#EdgeList[grepl("Q7184903)$", EdgeList$from),]
#EdgeList[grepl("Q16889133)$", EdgeList$from),]
#EdgeList[grepl("Q7184903)$", EdgeList$to),]
#EdgeList[grepl("Q16889133)$", EdgeList$to),]
EdgeList.noloop <- EdgeList[-c(154,183),]
#EdgeList.loop <- EdgeList
###########################################################################################
#CommunityAnalysis
library(igraph)
g <- simplify(graph_from_edgelist(as.matrix(EdgeList.noloop), directed = T))

#可視化
quartz(width=7.5, height=7.5)
set.seed(123)
plot(g,
     layout=layout_with_graphopt(g, niter = 5000),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = "skyblue",
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_02.pdf", sep=""), type = "pdf"); dev.off()

quartz(width=7.5, height=7.5)
set.seed(123)
plot(g,
     #layout=layout_as_tree(g, root=c(27), rootlevel=c(27), mode = "in"),
     layout=layout_as_tree(g, mode = "in"),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = "skyblue",
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_02_tree.pdf", sep=""), type = "pdf"); dev.off()

###########################################################################################
#Neighborhood of graph vertices
#subcomponent
###########################################################################################
#a1 <- make_ego_graph(g, order = 2, nodes = V(g), mode = "in", mindist = 0)
#a1.s <- ego_size(g, order = 2, nodes = V(g), mode = "in", mindist = 0)
#a1 <- a1[order(a1.s, decreasing = T)]

if(F){
library(igraph)
#include Loop
g <- simplify(graph_from_edgelist(as.matrix(EdgeList), directed = T))

Num00 <- c()
Num01 <- c()
for(n in 1:length(names(V(g)))){
Num00 <- c(Num00, length(names(subcomponent(g, n, "in"))))
Num01 <- c(Num01, length(names(subcomponent(g, n, "out"))))
}
Num <- data.frame(Num00, Num01)
NUM <- c(1:length(names(V(g))))[order(Num00, decreasing = T)]
g1 <- names(V(g))[order(Num00, decreasing = T)]
Num <- Num[order(Num00, decreasing = T),]

hist(Num00, breaks=35)

#graph output
for(n in 1:length(a1)){
#n <- 6
print(n)
if(Num00[order(Num00, decreasing = T)][n] > 4){
if(Num01[order(Num00, decreasing = T)][n] <= 4){

g2 <- g
g3i <- subcomponent(g, NUM[n], "in")
g3o <- subcomponent(g, NUM[n], "out")

V(g2)$community <- 1
V(g2)$community[names(V(g2)) %in% c(names(g3i), names(g3o))] <- 2

V(g2)$color <- "skyblue"
V(g2)$color[names(V(g2)) %in% names(g3o)] <- "lightgreen"
V(g2)$color[names(V(g2)) %in% g1[n]] <- "pink"

V(g2)$label.cex <- 0.06
V(g2)$label.cex[names(V(g2)) %in% g1[n]] <- 0.12

quartz(width=5, height=5)
set.seed(123)
par(cex.main=0.5)
plot(induced_subgraph(g2, V(g2)$community == 2),
     layout=layout_with_graphopt(induced_subgraph(g2, V(g2)$community == 2), niter = 5000),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.color="black",
     edge.arrow.size = 0.12,
     edge.curved=0,
     edge.arrow.width = 0.6)
#legend("top", legend=paste0("Top concept: ", g1[n]))
quartz.save(file = paste("./", Folder_out, "/Graph_02_", formatC(Num01[order(Num00, decreasing = T)][n], width=4, flag="0"), "_", formatC(n, width=4, flag="0"), "_1_", g1[n], "_nodes_num_", length(unique(c(names(g3i), names(g3o)))), ".pdf", sep=""), type = "pdf"); dev.off()

quartz(width=5, height=5)
set.seed(123)
par(cex.main=0.5)
plot(induced_subgraph(g2, V(g2)$community == 2),
     layout=layout_as_tree(induced_subgraph(g2, V(g2)$community == 2), mode = "in"),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.color="black",
     edge.arrow.size = 0.12,
     edge.curved=0,
     edge.arrow.width = 0.6)
quartz.save(file = paste("./", Folder_out, "/Graph_02_", formatC(Num01[order(Num00, decreasing = T)][n], width=4, flag="0"), "_", formatC(n, width=4, flag="0"), "_2_", g1[n], "_nodes_num_", length(unique(c(names(g3i), names(g3o)))), ".pdf", sep=""), type = "pdf"); dev.off()
}}}
}

#subcomponent
###########################################################################################
#Community strucure via short random walks
###########################################################################################
library(igraph)
#g <- simplify(graph_from_edgelist(as.matrix(EdgeList), directed = T))
g <- simplify(graph_from_edgelist(as.matrix(EdgeList.noloop), directed = T))

for(n in 1:10){
wc <- cluster_walktrap(g, steps = n)
print(paste0("n: ", n, " modularity: ", modularity(wc)))
}

wc <- cluster_walktrap(g, steps = 4)
table(membership(wc))
V(g)$community <-  membership(wc)
E(g)$weight = 1
#membership(wc)

#Save
Comm00 <- stringr::str_split_fixed(names(membership(wc)), pattern="[(]wd:|[)]", n=3)[,-3]
Comm <- data.frame(a=names(membership(wc)), b=Comm00[,1], c=paste0("wd:", Comm00[,2]), CommVal=as.numeric(membership(wc)))
readr::write_csv(Comm, file=paste0("./06_Out_txt/Comm.csv"), append=F, col_names = F)

g_grouped = g
for(i in unique(V(g)$community)){
  groupV = which(V(g)$community == i)
  g_grouped = add_edges(g_grouped, combn(groupV, 2), attr=list(weight = 1))
}

set.seed(123)
l <- layout_nicely(g_grouped)

quartz(width=7.5, height=7.5)
plot(wc, g,
     layout=l,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = "skyblue",
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v01.pdf", sep=""), type = "pdf"); dev.off()

set.seed(123)
quartz(width=7.5, height=7.5)
plot(g,
     layout=l,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(g)$community],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v02.pdf", sep=""), type = "pdf"); dev.off()

for(n in 1:max(V(g)$community)){
#n <- 1
set.seed(123)
quartz(width=5, height=5)
par(cex.main=0.75, family="HiraKakuProN-W3")
plot(induced_subgraph(g, V(g)$community == n),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 10, 
     edge.width = 0.5,
     vertex.label.cex = 0.2,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[n],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1,
  main=paste0("No.: ", n, ", Num.: ", length(names(V(induced_subgraph(g, V(g)$community == n))))))
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v03_", n, ".pdf", sep=""), type = "pdf"); dev.off()
}

######################################################
#Post-Community analysis
######################################################
a <- table(V(g)$community)
a <- a[order(a, decreasing = T)]

for(n in 1:length(a)){
#n <- 1
if(a[n] > 5){
b <- induced_subgraph(g, V(g)$community == names(a)[n])   

Num00 <- c()
Num01 <- c()
Num02 <- c()
for(m in 1:length(names(V(b)))){
#m <- 1
Num00 <- c(Num00, length(names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) == names(V(b))[m]], "in"))))
Num01 <- c(Num01, length(names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) == names(V(b))[m]], "out"))))
Num02 <- c(Num02, length(names(subcomponent(b, c(1:length(names(V(b))))[names(V(b)) == names(V(b))[m]], "out"))))
}

Dat00 <- data.frame(a=names(V(b)), b=1:length(names(V(b))), 
  c=Num00, d=Num01, e=Num02); Dat00 <- Dat00[order(Dat00$c, decreasing = T),]
head(Dat00)

#Top概念のみ
Dat01 <- Dat00[Dat00$e <= 2,]
head(Dat01)

#Num <- data.frame(Num00, Num01)
NUM <- Dat01$b
b1 <- Dat01$a
#Num <- Num[order(Num00, decreasing = T),]

for(k in 1:nrow(Dat01)){
#k <- 1
print(paste0("n: ", n, " k: ", k))
if(Dat01$c[k] > 4){

g2 <- g
g3i <- subcomponent(g, c(1:length(names(V(g))))[names(V(g)) == names(V(b))[NUM[k]]], "in")
  
V(g2)$community <- 1
V(g2)$community[names(V(g2)) %in% c(names(g3i))] <- 2

V(g2)$color <- "skyblue"
V(g2)$color[names(V(g2)) %in% b1[k]] <- "pink"

V(g2)$size <- 5
V(g2)$size[names(V(g2)) %in% b1[k]] <- 8

V(g2)$label.cex <- 0.06
V(g2)$label.cex[names(V(g2)) %in% b1[k]] <- 0.12

all <- length(names(V(b)))
sub <- length(names(g3i))
sub0 <- length(names(V(b))[names(V(b)) %in% names(g3i)])
all.sub0 <- round(sub0/all, 3)
sub.lab <- names(V(b))[!c(names(V(b)) %in% names(g3i))]

quartz(width=5, height=5)
set.seed(123)
par(cex.main=0.5, family="HiraKakuProN-W3")
plot(induced_subgraph(g2, V(g2)$community == 2),
     layout=layout_with_graphopt(induced_subgraph(g2, V(g2)$community == 2), niter = 5000),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     edge.width = 0.5,
     vertex.label.color="black",
     edge.arrow.size = 0.12,
     edge.curved=0,
     edge.arrow.width = 0.6,
  main=paste0(names(V(b))[NUM[k]], "\nCommunity: ", all, " sub: ", sub, " Ratio: ", all.sub0, "\n", paste(sub.lab, collapse = ",")))
#legend("bottom", legend=paste0("Top concept: ", b1[k]), cex=0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_sub_", formatC(n, width=4, flag="0") , "_", formatC(k, width=4, flag="0"), "_1_", b1[k], "_nodes_num_", length(unique(c(names(g3i)))), ".pdf", sep=""), type = "pdf"); dev.off()
################################################################
quartz(width=5, height=5)
set.seed(123)
par(cex.main=0.5, family="HiraKakuProN-W3")
try(plot(induced_subgraph(g2, V(g2)$community == 2),
     layout=layout_as_tree(induced_subgraph(g2, V(g2)$community == 2), mode = "in"),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     edge.width = 0.5,
     vertex.label.color="black",
     edge.arrow.size = 0.12,
     edge.curved=0,
     edge.arrow.width = 0.6,
  main=paste0(names(V(b))[NUM[k]], "\nCommunity: ", all, " sub: ", sub, " Ratio: ", all.sub0)), silent=T)
quartz.save(file = paste("./", Folder_out, "/Graph_sub_", formatC(n, width=4, flag="0") , "_", formatC(k, width=4, flag="0"), "_2_", b1[k], "_nodes_num_", length(unique(c(names(g3i)))), ".pdf", sep=""), type = "pdf"); dev.off()

#Community
gg2 <- induced_subgraph(g2, V(g2)$community == 2)
#gg2

a1 <- data.frame(a=names(V(gg2)), b=V(gg2)$community)
a2 <- data.frame(a=names(V(g)), c=V(g)$community)
a3 <- merge(a1, a2, by="a", sort=F)
#table(names(V(gg2)) == a3$a)
V(gg2)$community <- a3$c
V(gg2)$color <- rainbow(max(V(g)$community), alpha=0.5)[V(gg2)$community]

#V(gg2)$size <- 5
#V(gg2)$size[names(V(gg2)) %in% b1[k]] <- 8

#V(gg2)$label.cex <- 0.06
#V(gg2)$label.cex[names(V(gg2)) %in% b1[k]] <- 0.12

quartz(width=5, height=5)
set.seed(123)
par(cex.main=0.5, family="HiraKakuProN-W3")
plot(gg2,
     layout=layout_with_graphopt(gg2, niter = 5000),
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     edge.width = 0.5,
     vertex.label.color="black",
     edge.arrow.size = 0.12,
     edge.curved=0,
     edge.arrow.width = 0.6,
  main=paste0(names(V(b))[NUM[k]], "\nCommunity: ", all, " sub: ", sub, " Ratio: ", all.sub0, "\n", paste(sub.lab, collapse = ",")))
#legend("bottom", legend=paste0("Top concept: ", b1[k]), cex=0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_sub_comm_", formatC(n, width=4, flag="0") , "_", formatC(k, width=4, flag="0"), "_1_", b1[k], "_nodes_num_", length(unique(c(names(g3i)))), ".pdf", sep=""), type = "pdf"); dev.off()

abc <- data.frame(comm=names(a)[n], k=k, e=Dat01$e[k],
                  Top=names(V(b))[NUM[k]], Top.sub=sub, comm.num=a[n], Top.sub0=sub0, Top.ratio=all.sub0, 
                  exclude=paste0(sub.lab, collapse = ";"),
                  include=paste0(names(V(b))[names(V(b)) %in% names(g3i)], collapse=";"))

if(all(c(n == 1, k == 1))){
readr::write_excel_csv(abc, paste0(Folder_out, "/Intermediate_concepts_v01.csv"), col_names = F, append = F)  
}else{
readr::write_excel_csv(abc, paste0(Folder_out, "/Intermediate_concepts_v01.csv"), col_names = F, append = T)
}

}}
}
}


######################################################
#subgraph解析
######################################################
if(F){
plot(1:12, rep(1, 12), pch=21, cex=2, xlim=c(-6,12), bg=rainbow(max(V(g)$community), alpha=0.5))

for(n in 1:max(V(g)$community)){
a <- data.frame(cluster=n, Label=paste0(names(V(induced_subgraph(g, V(g)$community %in% n))), collapse = "; "))
readr::write_csv(a, paste0("./", Folder_out, "/Graph_subgraph.csv"), append=T, col_names=F)  
}

gg <- induced_subgraph(g, V(g)$community %in% c(1, 2, 3, 4, 5, 7, 9, 10:12))
gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1.5))
}

ll <- layout_nicely(gg_grouped)

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(gg)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v04.pdf", sep=""), type = "pdf"); dev.off()

##01
gg <- induced_subgraph(g, V(g)$community %in% c(1, 3, 7, 12))
gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

ll <- layout_nicely(gg_grouped)

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v05.pdf", sep=""), type = "pdf"); dev.off()

##02
gg <- induced_subgraph(g, V(g)$community %in% c(2, 4, 10))
gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

ll <- layout_nicely(gg_grouped)

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v06.pdf", sep=""), type = "pdf"); dev.off()

##03
gg <- induced_subgraph(g, V(g)$community %in% c(5, 9))

gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

l <- layout_nicely(gg_grouped)

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=l,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.1,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = 5, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v07.pdf", sep=""), type = "pdf"); dev.off()

###########################################################
#PageRank
g.pr <- page_rank(g, directed=TRUE)
range(g.pr$vector)
b1 <- log2(as.numeric(g.pr$vector)*1000)
range(b1)

#install.packages("CINNA")
library(CINNA)
centr_degree(g, mode = "all")$res
closeness(g, mode="all")
hist(closeness(g, mode="all"))

#中性性測定の比較解析
prop <- proper_centralities(g)[1:5]
a <- calculate_centralities(g, include = prop)
pca_centralities(a, scale.unit = TRUE)
a
names(a)
visualize_graph( g, centrality.type="Burt's Constraint")

b1 <- as.numeric(a$"Burt's Constraint")
range(b1)

b1 <- log2(as.numeric(a$"Page Rank")*1000)
range(b1)

################################################
set.seed(123)

quartz(width=7.5, height=7.5)
plot(g,
     layout=l,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.01,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = b1*1.25, 
     edge.width = 0.5,
     vertex.label.cex = 0.05,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(g)$community],
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v08.pdf", sep=""), type = "pdf"); dev.off()


################################################
##01
gg <- induced_subgraph(g, V(g)$community %in% c(1, 3, 7, 12))

gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

ll <- layout_nicely(gg_grouped)
c1 <- b1[names(V(g)) %in% names(V(gg))]

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.01,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = c1*2, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v09.pdf", sep=""), type = "pdf"); dev.off()

##02
gg <- induced_subgraph(g, V(g)$community %in% c(2, 4, 10))

gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

ll <- layout_nicely(gg_grouped)
c1 <- b1[names(V(g)) %in% names(V(gg))]

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.01,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = c1*2, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v10.pdf", sep=""), type = "pdf"); dev.off()

##03
gg <- induced_subgraph(g, V(g)$community %in% c(5, 9))

gg_grouped = gg
for(i in unique(V(gg)$community)){
  ggroupV = which(V(gg)$community == i)
  gg_grouped = add_edges(gg_grouped, combn(ggroupV, 2), attr=list(weight = 1))
}

ll <- layout_nicely(gg_grouped)
c1 <- b1[names(V(g)) %in% names(V(gg))]

set.seed(123)
quartz(width=7.5, height=7.5)
plot(gg,
     layout=ll,
     vertex.frame.color = "grey", 
     vertex.frame.width=0.01,
     vertex.label.family="HiraKakuProN-W3",
     vertex.size = c1*2, 
     edge.width = 0.5,
     vertex.label.cex = 0.1,
     vertex.label.color="black",
     vertex.color = rainbow(max(V(g)$community), alpha=0.5)[V(gg)$community],
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 0.5)
quartz.save(file = paste("./", Folder_out, "/Graph_comm_v11.pdf", sep=""), type = "pdf"); dev.off()
}


