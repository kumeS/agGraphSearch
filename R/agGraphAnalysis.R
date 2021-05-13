##' @title Graph analysis
##'
##' @param Entity_ID
##'
##'
##' @description this function is a general function for
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agGraphAnalysis
##' @examples \dontrun{
##'
##'
##' }

if(!require("igraph")){install.packages("igraph")}; library(igraph)

#graphList=eachGraph; list1=list1a; list2=list2b; UpdateList=F; breakRepeat=99; Entity_only=T; View=TRUE; RemoveGraph=F; PlusLabel=T; GraphView=T
#graphList=eachGraph; list1=list1a; list2=list2b; UpdateList=F; breakRepeat=99; View=TRUE; RemoveGraph=F; PlusLabel=T; GraphView=T; LvView=T; OutputResults=T; WindowSize01=10; WindowSize02=10; WindowSize03=10; WindowSize04=10; WindowSize05=10; FileName=T;FileName00="04_Results_01"; GraphView01=F;GraphView02=F;GraphView03=T; GraphView04=F

#PlusLabel=T; FileName=T; FileName00="04_Results_01"; LowerSearch=F

#list1: 検索エンティティ
#list2, list2.c, list2.wd: 共通上位エンティティ
#list3: 共通上位エンティティ + 展開段数
#graphList=eachGraph; list1=list1a; list2=list2b; LowerSearch=T;  UpdateList=F; breakRepeat=20;View=T; RemoveGraph=F; PlusLabel=T; FileName=T; FileName00="04_Results_01";OutputResults=T; ResultsEach=F; Entity_only=T; list2.WD="wd:Q35120"; GraphView01=F; GraphView02=F; GraphView03=T; GraphView04=T; LvView=T;WindowSize01=10; WindowSize02=10;WindowSize03=10; WindowSize04=7.5;WindowSize05=7.5

agGraphAnalysis <- function(graphList=eachGraph, list1=list1a, list2=list2b,
                              UpdateList=F, breakRepeat=20,
                              View=T, RemoveGraph=F,
                              PlusLabel=T, FileName=T, FileName00="04_Results_01",
                              OutputResults=T, ResultsEach=F, LowerSearch=F,
                              Entity_only=T, list2.WD="wd:Q35120",
                              GraphView01=F, GraphView02=F, GraphView03=T, GraphView04=T, LvView=T,
                              WindowSize01=10, WindowSize02=10,
                              WindowSize03=10, WindowSize04=7.5,
                              WindowSize05=7.5){
#graphListの型確認
if(is.list(graphList)){}else{return(message("graphList is not the list type"))}

#共通エンティティ
if(Entity_only){
    list2.c <- list2
    list3 <- data.frame(V1=list2.c, V2=NA)
    list2.wd <- list2.WD
}else{
    list2.c <- list2[!(list2 %in% "wd:Q35120")]
    list3 <- data.frame(V1=list2.c, V2=NA)
    list2.wd <- list2.c
}

#検索エンティティから共通エンティティを除外
list1.c <- list1[!(list1 %in% list2.c)]

#length(graphList)
graphList00 <- c()
for(nn in seq_len(length(graphList))){
#nn <-1
if(PlusLabel){
graphList00[[nn]] <- data.frame(from=graphList[[nn]]$subject,
                              to=graphList[[nn]]$parentClass,
                              fromLab=gsub(" ", "_", paste0(graphList[[nn]]$subject, ".", graphList[[nn]]$subjectLabel)),
                              toLab=gsub(" ", "_", paste0(graphList[[nn]]$parentClass, ".", graphList[[nn]]$parentClassLabel)),
                              stringsAsFactors = F)
}else{
graphList00[[nn]] <- data.frame(from=graphList[[nn]]$subject,
                              to=graphList[[nn]]$parentClass,
                              stringsAsFactors = F)
}}

#出力先の作成
if(FileName){Folder <- FileName00}else{Folder <- paste("Results_", format(Sys.time(), "%y%m%d_%H%M"), sep="")}
if(!exists(Folder)){dir.create(Folder)}
FolderData <- paste(Folder, "/SaveData", sep="")
if(!exists(FolderData)){dir.create(FolderData)}

#共通エンティティごとに処理実施
for(p in seq_len(length(list2.wd))){
#p <- 1
#p <- 416
if(View){print(paste(p, "step 01"))}
A <- c()
lab <- as.character(list2.wd[p]); lab.c <- lab

#グラフの取り出し
Y00 <- graphList00

#エッジリストの結合
B <- c()
for(m in seq_len(length(Y00))){
 B <- B %>% rbind(Y00[[m]])
}

#head(B)
#table(is.na(B))
rownames(B) <- 1:nrow(B)

#Bが存在しないなら、スキップ
if(is.null(B)){
list3[list3$V1 == lab.c, 2] <- NA
}else{
#重複除く
C <- data.frame(B, paste(B$from, ".", B$to, sep=""))
colnames(C) <- c(colnames(B), "unique")
C <- C[as.numeric(rownames(unique(C["unique"]))),]
C <- C[,-ncol(C)]

#ループ除く
C <- C[C$from != C$to,]
#head(C)
#c(C$from, C$to)

#ループ除く
head(C)
C$V1 <- paste0(C$from, ".", C$to)
C$V2 <- paste0(C$to, ".", C$from)
C$V3 <- 0
for(l in seq_len(nrow(C))){
#l <- 1
if(any(c(C$V1[l] == C$V2))){C$V3[l] <- 1}
if(any(c(C$V2[l] == C$V1))){C$V3[l] <- 1}
}

#head(C); table(C$V3)
C <- C[C$V3 == 0,]
C <- C[,1:4]
rownames(C) <- 1:nrow(C)
#head(C)

#エンティティ数の不一致検出
message("Entity count mismatch detection")
print(paste0("length(unique(C$from)): ", length(unique(C$from))))
print(paste0("length(unique(C$fromLab)): ", length(unique(C$fromLab))))
print(paste0("length(unique(C$to)): ", length(unique(C$to))))
print(paste0("length(unique(C$toLab)): ", length(unique(C$toLab))))

if(length(unique(C$from)) != length(unique(C$fromLab))){
a <- map(unique(C$from), function(x){ unique(C[C$from == x, 3])[length(unique(C[C$from == x, 3])) > 1] })
b <- a[!unlist(map(a, function(x){length(unlist(x)) == 0}))]
for(k in seq_len(length(b))){
  #k <- 1
  for(i in seq_len(length(b[[k]]))){
  #i <- 2
  if(i != 1){C$fromLab[C$fromLab == b[[k]][i]] <- b[[k]][1]}
  }}
message("Retry Detection")
print(paste0("length(unique(C$from)): ", length(unique(C$from))))
print(paste0("length(unique(C$fromLab)): ", length(unique(C$fromLab))))
}

if(length(unique(C$to)) != length(unique(C$toLab))){
a <- map(unique(C$to), function(x){ unique(C[C$to == x, 4])[length(unique(C[C$to == x, 4])) > 1] })
b <- a[!unlist(map(a, function(x){length(unlist(x)) == 0}))]
for(k in seq_len(length(b))){
  #k <- 1
  for(i in seq_len(length(b[[k]]))){
  #i <- 2
  if(i != 1){C$toLab[C$toLab == b[[k]][i]] <- b[[k]][1]}
  }}
print(paste0("length(unique(C$to)): ", length(unique(C$to))))
print(paste0("length(unique(C$toLab)): ", length(unique(C$toLab))))
}

#結合グラフの保存
saveRDS(C, file = paste("./", FolderData, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c,
  "_A.Rdata", sep=""), compress = TRUE)
write.table(C, file = paste("./", FolderData, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c,
  "_A.csv", sep=""), row.names = F, sep=",")

#グラフオブジェクトに変換
if(PlusLabel){
D00 <- graph_from_edgelist(as.matrix(C)[,3:4], directed = T)
D <- graph_from_edgelist(as.matrix(C)[,1:2], directed = T)
D00.d <- decompose(D00, min.vertices =1)
D.d <- decompose(D, min.vertices =1)

for(k in seq_len(length(D.d))){
#k <- 1
D00.d1 <- D00.d[[k]]
D.d1 <- D.d[[k]]

Col00 <- c(attr(V(D.d1), "names"))
Col01 <- rep("grey", length(Col00))
Col01[Col00 %in% list1.c] <- "lightblue"
Col01[Col00 %in% lab.c] <- "lightgreen"
Size01 <- rep(1.5, length(Col00))
Size01[Col00 %in% list1.c] <- 3
Size01[Col00 %in% lab.c] <- 6
Size02 <- rep(0.1, length(Col00))
Size02[Col00 %in% list1.c] <- 0.1
Size02[Col00 %in% lab.c] <- 0.12
Col00 <- c(attr(V(D00.d1), "names"))
FONT <- "HiraKakuProN-W3"

#可視化
if(GraphView01){
quartz(width=WindowSize01, height=WindowSize01)
set.seed(123)
plot(D.d1,
     #layout =layout_as_tree(D01, mode = "in"),
     layout=layout_with_fr(D.d1),
     vertex.frame.color = Col01,
     vertex.frame.width=0.1,
     vertex.label.family=FONT,
     vertex.label=Col00,
     vertex.size = Size01,
     edge.width = 0.5,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     #vertex.label=Dat$Nodes,
     #vertex.label=paste(Dat$Nodes, "\n(", Dat$Comm, ", ",Dat$Comm2, ")", sep=""),
     #vertex.label=NA,
     vertex.color = Col01,
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
title(paste("Common: ", lab.c,
            "\nGraph Num.: ", length(A[c(A == TRUE)]), sep=""),
      cex.main=1, col.main="black")
quartz.save(file = paste("./", Folder, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c, "_", formatC(k, width = 3, flag = "0"), "_01.pdf", sep=""),
            type = "pdf"); dev.off()
}
}
}else{
D <- graph_from_edgelist(as.matrix(C), directed = T)
Col00 <- c(attr(V(D), "names"))
Col01 <- rep("grey", length(Col00))
Col01[Col00 %in% list1.c] <- "lightblue"
Col01[Col00 %in% lab.c] <- "lightgreen"
Size01 <- rep(1.5, length(Col00))
Size01[Col00 %in% list1.c] <- 3
Size01[Col00 %in% lab.c] <- 6
Size02 <- rep(0.1, length(Col00))
Size02[Col00 %in% list1.c] <- 0.2
Size02[Col00 %in% lab.c] <- 0.25
FONT <- "Times"

#可視化
if(GraphView01){
quartz(width=WindowSize01, height=WindowSize01)
set.seed(123)
plot(D,
     #layout =layout_as_tree(D01, mode = "in"),
     layout=layout_with_fr(D),
     vertex.frame.color = Col01,
     vertex.frame.width=0.1,
     vertex.label.family=FONT,
     vertex.label=Col00,
     vertex.size = Size01,
     edge.width = 0.5,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     #vertex.label=Dat$Nodes,
     #vertex.label=paste(Dat$Nodes, "\n(", Dat$Comm, ", ",Dat$Comm2, ")", sep=""),
     #vertex.label=NA,
     vertex.color = Col01,
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
title(paste("Common: ", lab.c,
            "\nGraph Num.: ", length(A[c(A == TRUE)]), sep=""),
      cex.main=1, col.main="black")
quartz.save(file = paste("./", Folder, "/Graph_", "_", lab.c, "_01_", formatC(p, width = 4, flag = "0"), ".pdf", sep=""),
            type = "pdf"); dev.off()
}
}

if(View){print(paste(p, "step 02"))}
#隣接行列に変換
E <- as.data.frame(as.matrix(as_adj(D)))

#共通エンティティの下位をすべて取得
#LowerSearch =T
if(LowerSearch){
G <- c()
x <- 0
repeat{
H <- c()
x <- x + 1
for(m in seq_len(length(lab))){
#m <- 1
E_1 <- E[colnames(E) == lab[m]]
E_2 <- rownames(E_1)[E_1 == 1]

if(identical(E_2, character(0))){}else{
for(n in seq_len(length(E_2))){
H <- H %>% rbind(data.frame(from=E_2[n], to=lab[m], stringsAsFactors = F))
}}}

if(is.null(H)){break}else{
lab <- H[,1]
G <- G %>% rbind(H)
}
if(x > as.numeric(breakRepeat)){break}else{}
}
}else{
#head(G)
G <- C[,1:2]
}

#head(G)
#ループ除く
G_loop <- data.frame(G, paste(G$from, ".", G$to, sep=""))
colnames(G_loop) <- c("from", "to", "unique")
G_loop <- G_loop[as.numeric(rownames(unique(G_loop["unique"]))),]
G_loop <- G_loop[,1:2]

##グラフの修正
##トリプルの関係が共通エンティティ同士なら除外
G_mod <- G_loop
G_mod$from <- G_loop$from %in% list2.c
G_mod$to <- G_loop$to %in% list2.c
G_mod00 <- G_loop[(!apply(G_mod, 1, all)),]

#保存
saveRDS(G_loop, file = paste("./", FolderData, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c,
  "_B.Rdata", sep=""), compress = TRUE)
write.table(G_loop, file = paste("./", FolderData, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c,
  "_B.csv", sep=""), row.names = F, sep=",")

if(View){print(paste(p, "step 03"))}
###最短パスの計算
#head(G_mod00)
G.path00 <- G_mod00

if(dim(G.path00)[1] != 0){
#ラベル結合(1)
head(G_loop01 <- G_loop)
G_loop01$from.to <- paste(G_loop01$from, ".", G_loop01$to, sep="")
head(C01 <- C)
C01$from.to <- paste(C01$from, ".", C01$to, sep="")
#head(G_loop01); head(C01)
#dim(G_loop01); dim(C01)
G_loop02 <- merge(G_loop01, C01, by="from.to", all = F, sort=F)
head(G_loop02)
dim(G_loop02)
G_loop03 <- G_loop02[,c(2:3,6:7,1)]
colnames(G_loop03) <- c("from","to","fromLab", "toLab","from.to")
head(G_loop03)
table(is.na(G_loop03))
dim(G_loop03)

#ラベル結合(2)
head(G.path01 <- G.path00)
G.path01$from.to <- paste(G.path01$from, ".", G.path01$to, sep="")
head(C01 <- C)
C01$from.to <- paste(C01$from, ".", C01$to, sep="")
#head(G.path01); head(C01)
#dim(G.path01); dim(C01)
G.path02 <- merge(G.path01, C01, by="from.to", all = F, sort=F)
head(G.path02)
G.path03 <- G.path02[,c(2:3,6:7,1)]
colnames(G.path03) <- c("from","to","fromLab", "toLab","from.to")
head(G.path03)
table(is.na(G.path03))

#a <- map(unique(G.path03$from), function(x){ unique(G.path03[G.path03$from == x, 3])[length(unique(G.path03[G.path03$from == x, 3])) > 1] })
#b <- a[!unlist(map(a, function(x){length(unlist(x)) == 0}))]

#グラフオブジェクトに変換
G.path <- graph_from_edgelist(as.matrix(G.path00), directed = T)

#リストの更新
list4.g <- as.character(unlist(G_mod00))
list1.g <- list1.c[list1.c %in% list4.g]

if(ResultsEach){
list2.g <- list2.wd[p]
}else{
list2.g <- list2.c[list2.c %in% list4.g]
}

message(paste0("Number of expand common entity: ", length(list2.g)))

#Lmaxの計算
#head(list2.g)
for(j in seq_len(length(list2.g))){
#j <- 1
Z <- c()
if(View){print(paste(p, "step 04 ", j))}

for(l in seq_len(length(list1.g))){
#l <- 1
suppressWarnings(try(len <- length(as_ids(shortest_paths(G.path,
  from = as.character(list1.g[l]), to = as.character(list2.g[j]))$vpath[[1]])) - 1, silent=T))
if(is.null(len) | len < 0){}else{Z <- c(Z, len)}
if(UpdateList){list1.c <- list1.c[list1.c != list1.g[l]]}else{}
}

if(suppressWarnings(identical(max(Z), -Inf))){}else{
if(is.na(list3[list3$V1 == list2.g[j], 2])){
list3[list3$V1 == list2.g[j], 2] <- max(Z)
}else{
if(list3[list3$V1 == list2.g[j], 2] < max(Z)){
list3[list3$V1 == list2.g[j], 2] <- max(Z)
}else{}
}}
if(OutputResults){
list4 <- list3
colnames(list4) <- c("CommonEntity", "Levels")
saveRDS(list4, file=paste("./", Folder, "/Results_output.Rdata", sep=""))
}
}

#list3.p <- list3[list3$V1 %in% list2.g,]; table(is.na(list3$V2))
#SearchLevelsも追加
head(G.path04 <- G.path03)
#table(!is.na(list3$V2))
list3.SearchLevels <- list3[!is.na(list3$V2),]
for(n in seq_len(nrow(list3.SearchLevels))){
  # n <- 1
  G.path04$fromLab[G.path04$from == list3.SearchLevels[n,1]] <- paste(G.path04$fromLab[G.path04$from == list3.SearchLevels[n,1]],
                                                                      ".L", list3.SearchLevels[n,2], sep="")
  G.path04$toLab[G.path04$to == list3.SearchLevels[n,1]] <- paste(G.path04$toLab[G.path04$to == list3.SearchLevels[n,1]],
                                                                  ".L", list3.SearchLevels[n,2], sep="")
}

if(LvView){G.path03 <- G.path04}
head(G.path03)

##可視化1
if(PlusLabel){
head(G_loop03)
#apply(G_loop03, 2, function(x){length(unique(x))})
#length(unique(paste0(G_loop03[,1], ".", G_loop03[,2])))
#length(unique(paste0(G_loop03[,3], ".", G_loop03[,4])))
Glab <- graph_from_edgelist(as.matrix(G_loop03[,3:4]), directed = T)
G00 <- graph_from_edgelist(as.matrix(G_loop03[,1:2]), directed = T)

Col00 <- c(attr(V(G00), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
list2.ccc <- list3$V1[!is.na(list3$V2)]
Col01[Col00 %in% list2.ccc] <- "pink"

Size01 <- rep(1, length(Col00))
Size01[Col00 %in% list1.c] <- 1.5
Size01[Col00 %in% list2.cc] <- 1.5
Size02 <- rep(0.05, length(Col00))
Size02[Col00 %in% list1.c] <- 0.05
Size02[Col00 %in% list2.cc] <- 0.05
FONT <- "HiraKakuProN-W3"

if(length(Col00) == length(c(attr(V(Glab), "names")))){
Col00 <- c(attr(V(Glab), "names"))
}else{
aa <- c(attr(V(Glab), "names"))
bb <- map(aa, function(x){str_split(x, pattern="\\.")[[1]]})
for(n in seq_len(length(bb))){
#n <- 1
Col00[Col00 == bb[[n]][1]] <- aa[n]
}
}
}else{
G00 <- graph_from_edgelist(as.matrix(G_loop), directed = T)
Col00 <- c(attr(V(G00), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
list2.ccc <- list3$V1[!is.na(list3$V2)]
Col01[Col00 %in% list2.ccc] <- "pink"

Size01 <- rep(1, length(Col00))
Size01[Col00 %in% list1.c] <- 1.5
Size01[Col00 %in% list2.cc] <- 1.5
Size02 <- rep(0.01, length(Col00))
Size02[Col00 %in% list1.c] <- 0.01
Size02[Col00 %in% list2.cc] <- 0.01
FONT <- "Times"
}

if(GraphView02){
quartz(width=WindowSize02, height=WindowSize02)
set.seed(123)
plot(G00,
     #layout =layout_as_tree(D01, mode = "in"),
     layout=layout_with_fr(G00),
     vertex.frame.color = Col01,
     vertex.frame.width=0.1,
     vertex.label.family=FONT,
     vertex.label=Col00,
     vertex.size = Size01,
     edge.width = 0.5,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     #vertex.label=Dat$Nodes,
     #vertex.label=paste(Dat$Nodes, "\n(", Dat$Comm, ", ",Dat$Comm2, ")", sep=""),
     #vertex.label=NA,
     vertex.color = Col01,
     edge.arrow.size = 0.15,
     edge.curved=0,
     edge.arrow.width = 1)
#title(paste("Common: ", lab.c,
#            "\nGraph Num.: ", length(A[c(A == TRUE)]), sep=""),
#      cex.main=1, col.main="black")
quartz.save(file = paste("./", Folder, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c, "_02.pdf", sep=""),
            type = "pdf"); dev.off()
}

##可視化2
if(nrow(G.path03) != 1){
if(PlusLabel){
head(G.path03)
#apply(G.path03, 2, function(x){length(unique(x))})

Glab <- graph_from_edgelist(as.matrix(G.path03)[,3:4], directed = T)
G00_mod <- graph_from_edgelist(as.matrix(G.path03[,1:2]), directed = T)

Col00 <- c(attr(V(G00_mod), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
list2.ccc <- list3$V1[!is.na(list3$V2)]
Col01[Col00 %in% list2.ccc] <- "pink"

Size01 <- rep(1, length(Col00))
Size01[Col00 %in% list1.c] <- 1.5
Size01[Col00 %in% list2.cc] <- 1.5
Size02 <- rep(0.05, length(Col00))
Size02[Col00 %in% list1.c] <- 0.05
Size02[Col00 %in% list2.cc] <- 0.05
FONT <- "HiraKakuProN-W3"

if(length(Col00) == length(c(attr(V(Glab), "names")))){
Col00 <- c(attr(V(Glab), "names"))
}else{
aa <- c(attr(V(Glab), "names"))
bb <- map(aa, function(x){str_split(x, pattern="\\.")[[1]]})
for(n in seq_len(length(bb))){
#n <- 1
Col00[Col00 == bb[[n]][1]] <- aa[n]
#if(any(Col00 == bb[[n]][1])){print(aa[n])}
}}

}else{
G00_mod <- graph_from_edgelist(as.matrix(G_mod00), directed = T)
Col00 <- c(attr(V(G00_mod), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
Size01 <- rep(2, length(Col00))
Size01[Col00 %in% list1.c] <- 2.5
Size01[Col00 %in% list2.cc] <- 2.5
Size02 <- rep(0.05, length(Col00))
Size02[Col00 %in% list1.c] <- 0.05
Size02[Col00 %in% list2.cc] <- 0.05
FONT <- "Times"
}

if(GraphView03){
quartz(width=WindowSize03, height=WindowSize03)
set.seed(123)
plot(G00_mod,
     #layout =layout_as_tree(G00, mode = "in"),
     layout=layout_with_fr(G00_mod),
     vertex.frame.color = Col01,
     vertex.frame.width=0.1,
     vertex.label.family=FONT,
     vertex.label=Col00,
     vertex.size = Size01,
     edge.width = 0.5,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     #vertex.label=Dat$Nodes,
     #vertex.label=paste(Dat$Nodes, "\n(", Dat$Comm, ", ",Dat$Comm2, ")", sep=""),
     #vertex.label=NA,
     vertex.color = Col01,
     edge.arrow.size = 0.2,
     edge.curved=0,
     edge.arrow.width = 1)
#title(paste("Common: ", lab.c,
#            "\nGraph Num.: ", length(A[c(A == TRUE)]), sep=""),
#      cex.main=1, col.main="black")
quartz.save(file = paste("./", Folder, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c, "_03.pdf", sep=""),
            type = "pdf"); dev.off()
}

#個別グラフ
if(GraphView04){
if(PlusLabel){
head(G.path03)
#table(G.path03$from == unlist(map(G.path03$fromLab, function(x){str_split(x, pattern="\\.")[[1]][1]})))
#table(G.path03$to == unlist(map(G.path03$toLab, function(x){str_split(x, pattern="\\.")[[1]][1]})))
#apply(G.path03, 2, function(x){length(unique(x))})

Glab <- graph_from_edgelist(as.matrix(G.path03)[,3:4], directed = T)
G00_mod <- graph_from_edgelist(as.matrix(G.path03[,1:2]), directed = T)
G00_mod.dg <- decompose(G00_mod, min.vertices = 2)
aa <- c(attr(V(Glab), "names"))
bb <- map(aa, function(x){str_split(x, pattern="\\.")[[1]]})

for(N in seq_len(length(G00_mod.dg))){
#N <- 1
G00_mod.dg00 <- G00_mod.dg[[N]]
Col00 <- c(attr(V(G00_mod.dg00), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
list2.ccc <- list3$V1[!is.na(list3$V2)]
Col01[Col00 %in% list2.ccc] <- "pink"

if(N == 1){
  b=WindowSize04; cex000=1; cex001=1.75; cex002=0.01
}else{
  b=WindowSize05; cex000=5; cex001=10; cex002=0.1
}

Size01 <- rep(cex000, length(Col00))
Size01[Col00 %in% list1.c] <- cex001
Size01[Col00 %in% list2.cc] <- cex001
Size02 <- rep(cex002, length(Col00))
Size02[Col00 %in% list1.c] <- cex002
Size02[Col00 %in% list2.cc] <- cex002
FONT <- "HiraKakuProN-W3"

if(length(Col00) == length(c(attr(V(Glab), "names")))){}else{
for(n in seq_len(length(bb))){
#n <- 1
Col00[Col00 == bb[[n]][1]] <- aa[n]
}}

quartz(width=b, height=b)
set.seed(123)
plot(G00_mod.dg00,
     #layout =layout_as_tree(G00, mode = "in"),
     layout=layout_with_fr(G00_mod.dg00),
     vertex.frame.color = Col01,
     vertex.frame.width=0.1,
     vertex.label.family=FONT,
     vertex.label=Col00,
     vertex.size = Size01,
     edge.width = 0.2,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     #vertex.label=Dat$Nodes,
     #vertex.label=paste(Dat$Nodes, "\n(", Dat$Comm, ", ",Dat$Comm2, ")", sep=""),
     #vertex.label=NA,
     vertex.color = Col01,
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 1)
quartz.save(file = paste("./", Folder, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c, "_04_", N, ".pdf", sep=""),
            type = "pdf"); dev.off()
}
}else{
G00_mod.dg <- decompose(G00_mod, min.vertices = 2)

for(N in seq_len(length(G00_mod.dg))){
#N <- 1
G00_mod.dg00 <- G00_mod.dg[[N]]
Col00 <- c(attr(V(G00_mod.dg00), "names"))
Col01 <- rep("grey", length(Col00))

Col01[Col00 %in% list1.c] <- "lightblue"
list2.cc <- list2.c[!(list2.c %in% list1.c)]
Col01[Col00 %in% list2.cc] <- "lightgreen"
Size01 <- rep(2, length(Col00))
Size01[Col00 %in% list1.c] <- 2.5
Size01[Col00 %in% list2.cc] <- 2.5
Size02 <- rep(0.1, length(Col00))
Size02[Col00 %in% list1.c] <- 0.2
Size02[Col00 %in% list2.cc] <- 0.2

pdf(file = paste("./", Folder, "/Graph_", formatC(p, width = 4, flag = "0"), "_", lab.c, "_04_", N, ".pdf", sep=""),
    width=WindowSize04, height=WindowSize04)
plot(G00_mod.dg00,
     layout=layout_with_fr(G00_mod.dg00)*2,
     vertex.frame.color = Col01,
     vertex.size = Size01,
     edge.width = 0.5,
     vertex.label.cex = Size02,
     vertex.label.color="black",
     vertex.color = Col01,
     edge.arrow.size = 0.1,
     edge.curved=0,
     edge.arrow.width = 1)
dev.off()
}
}
}
}
}
}
}
colnames(list3) <- c("CommonEntity", "Levels")
return(list3)
}

