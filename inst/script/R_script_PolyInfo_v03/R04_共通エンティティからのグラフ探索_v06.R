rm(list=ls())
source("../00_R_Script_gist/Common_Entity_Graph_Analysis_v05.R")
if(!dir.exists("04_Out")){dir.create("04_Out")}
#################################################################################

options(max.print=999999)
library(stringr)
library(purrr)
#install.packages("furrr")
library(furrr)
library(progress)
library(VennDiagram)
library(beepr)
library(igraph)

#個別グラフ
eachGraph <- readRDS("./03_Out/R03_wikiGraph_01_up.Rdata")

head(eachGraph[[1]])
length(eachGraph)
dim(eachGraph[[1]])
sapply(eachGraph, dim)

#ループ事例
#N <- 35
#eachGraph[[N]]
#eachGraph[[N]]$subject == eachGraph[[N]]$parentClass
#eachGraph[[N]][29,]

#検索エンティティ
QID_rdfsLabel <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")[,-3]
head(QID_rdfsLabel)
readr::write_excel_csv(QID_rdfsLabel, file = "./04_Out/R04_QID_rdfsLabel.csv")

dim(QID_rdfsLabel)
list1a <- unique(QID_rdfsLabel$QID)
head(list1a)
any(list1a == "wd:Q35120")

#共通エンティティ
list2a <- readRDS("./03_Out/R03_pc_table2.Rdata")

head(list2a)
dim(list2a)
unique(list2a$Freq)
list2b <- unique(list2a$parentClass)
head(list2b)
any(list2b == "wd:Q35120")

#Q35120を共通リストから除外
list2b <- list2b[list2b != "wd:Q35120"]

##包含
table(list1a %in% list2b)
table(list2b %in% list1a)

#################################################################################
##まずは、全体で動作確認
#################################################################################
##グラフアルゴリズム
#graphList=eachGraph; list1=list1a; list2=list2b
#list2b="wd:Q35120"
head(eachGraph[[1]])

#計算
system.time(SearchNum <- Graph_Analysis_v3(graphList=eachGraph, list1=list1a, list2=list2b, LowerSearch=T))
head(SearchNum)
table(SearchNum$Levels)
sum(table(SearchNum$Levels))
table(SearchNum$Levels)
table(!is.na(SearchNum[,2]))

#################################################################################
Nam <- paste0("./04_Out/SearchNum_", format(Sys.time(), "%y%m%d"))
saveRDS(SearchNum, file = paste0(Nam, ".Rdata"), compress = TRUE)
readr::write_excel_csv(SearchNum, file = paste0(Nam, ".csv"))
#################################################################################
#################################################################################
rm(list=ls())
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")
xx <- dir("./04_Out", pattern = "SearchNum_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
SearchNum <- readRDS(xx[length(xx)])

head(SearchNum)
dim(SearchNum)
table(is.na(SearchNum$Levels))
table(SearchNum$Levels)

##ラベル検索
library(furrr)
plan(multisession(workers = 4))
plan()

##ラベル検索
Lab <- future_map(SearchNum$CommonEntity, agQIDtoLabel, .progress = TRUE)
table(unlist(map(Lab, function(x) { nrow(x) })))
Lab00 <- c(); for(n in 1:length(Lab)){Lab00 <- Lab00 %>% rbind(Lab[[n]])}

head(Lab00)
dim(Lab00)

SearchNum00 <- data.frame(SearchNum, Lab00)
head(SearchNum00)
dim(SearchNum00)
table(SearchNum00$CommonEntity == SearchNum00$QID)

#保存
saveRDS(SearchNum00, file = paste0("./04_Out/SearchNum00_", format(Sys.time(), "%y%m%d"), ".Rdata"), compress = TRUE)

############################################################
#（１）展開段数のカウント
############################################################
rm(list=ls())
xx <- dir("./04_Out", pattern = "SearchNum00_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
Count <- readRDS(xx[length(xx)])
head(Count)

table(is.na(Count$Levels))
CountR <- Count[!is.na(Count$Levels),]
table(is.na(CountR$Levels))

head(CountR)
dan <- as.numeric(names(table(CountR$Levels)))
abc <- data.frame(V1=1:max(dan))
abc$V2[dan] <- table(CountR$Levels)
abc$V2[is.na(abc$V2)] <- 0

library(plotly)
fig <- plot_ly(abc, x = ~V1, y = ~V2, type = 'bar', name = '',
               hovertemplate = paste('%{y}', sep=""),
               marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% layout(title = paste("展開段数 v.s. 展開共通上位エンティティ数（QID数)", sep=""),
                      yaxis = list(title = 'QID数のカウント', linewidth = 1),
                      xaxis = list(title = "展開段数", ticks = "outside", linewidth = 0,
                                     tick0 = 1, dtick = 1),
                      margin = list(l=50, r=10, b=50, t=50, pad=4))
fig

Nam <- paste0("./Barplot_results_", format(Sys.time(), "%y%m%d"),".html")
htmlwidgets::saveWidget(plotly::as_widget(fig), Nam )
system(paste0("mv -f '", Nam, "' ./04_Out"))

library(knitr)
abc
colnames(abc) <- c("展開段数", "展開共通上位エンティティ数（QID数)")
(a <- kable(abc, format = "markdown"))
############################################################
############################################################
#結果まとめ
#エンティティからの段数評価
############################################################
rm(list=ls())

#起点検出
xx <- dir("./04_Out", pattern = "SearchNum00_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
Count <- readRDS(xx[length(xx)])
head(Count)
table(Count$Levels, useNA = "ifany")

#共通エンティティ
list2a <- readRDS("./03_Out/R03_pc_table2.Rdata")
head(list2a)
rownames(list2a) <- 1:nrow(list2a)
colnames(list2a) <- c("CommonEntity", "Freq", "EntityLabel")
Count00 <- merge(Count, list2a, by="CommonEntity", sort = F)
head(Count00)

Count01 <- Count00[!is.na(Count00$Levels),]
Count01 <- Count01[order(Count01$Levels),]
head(Count01)
Count01$EntityEval <- NA
#table(Count01$entityNamej == Count01$EntityLabel)
colnames(Count01)
Count02 <- Count01[,c(1, 6, 2, 3:5, 8)]
head(Count02)
 
#ファイル読み込み
file_path <- "./wikidata201013_v01_copy/Relation_P31_P279_df.csv"
a <- data.frame(readr::read_csv(file_path, col_names = F))

#エンティティまでの段数評価
for( n in 1:nrow(Count02) ){
#n <- 1

#上位検索
message(paste0("###############\nNo: ", n))
message(paste0("CommonEntity: ", Count02$CommonEntity[n]))
Query <- Count02$CommonEntity[n]
x <- 0

repeat{
x <- x +1
message(paste0("up: ", x))
c <- c(a[,1] %in% Query)
b <- a[c,]

if(any(b$X3 == "wd:Q35120")){
#head(Count02)
Count02$EntityEval[n] <- x
message(paste0("Output: ", x))
break
}
Query <- b$X3
}
}

head(Count02)
table(Count02$EntityEval, useNA = "ifany")
readr::write_excel_csv(Count02, file = paste0("./04_Out/SearchNum_count02_", format(Sys.time(), "%y%m%d"), ".csv"))

###############################################################
###############################################################
#EntityEvalの評価
rm(list=ls())

xx <- dir("./04_Out", pattern = "SearchNum_count02_[1-9][0-9][0-9][0-9][0-9][0-9].csv", full.names = T)
Count <- data.frame(readr::read_csv(xx[length(xx)]))
head(Count)

#EntityEval
table(Count$EntityEval)

for(n in 1:max(Count$EntityEval)){
#n <- 1
Count00 <- Count[Count$EntityEval == n,]
head(Count00)
print(n)
print(paste0(Count00$entityNamej, "(", Count00$CommonEntity, ")", collapse = ", "))
NN <- data.frame(paste0(Count00$entityNamej, "(", Count00$CommonEntity, ")", collapse = ", "))

if(n == 1){
readr::write_excel_csv(data.frame(paste0("Depth from Entity: ", n)), file = paste0("./04_Out/EntityEval_list_", format(Sys.time(), "%y%m%d"), ".csv"), append = F, col_names = F)
readr::write_excel_csv(NN, file = paste0("./04_Out/EntityEval_list_", format(Sys.time(), "%y%m%d"), ".csv"), append = T, col_names = F)
}else{
readr::write_excel_csv(data.frame(paste0("Depth from Entity: ", n)), file = paste0("./04_Out/EntityEval_list_", format(Sys.time(), "%y%m%d"), ".csv"), append = T, col_names = F)
readr::write_excel_csv(NN, file = paste0("./04_Out/EntityEval_list_", format(Sys.time(), "%y%m%d"), ".csv"), append = T, col_names = F)
}
}

############################################################
#エンティティ(最上位)からの下位検索
############################################################
if(F){
rm(list=ls())

#ファイル読み込み
file_path <- "./wikidata201013_v01_copy/Relation_P31_P279_df.csv"
a <- data.frame(readr::read_csv(file_path, col_names = F))

#下位検索
Query <- "wd:Q35120"
x <- 0
#Proc <- "both"
#Proc <- "wdt:P279"
Proc <- "wdt:P31"
N <- 6

repeat{
x <- x +1
message(paste0("down: ", x))

cc <- c(a[,3] %in% Query)
b <- a[cc,]
switch(Proc,
   "both" = b <- b,
   "wdt:P279" = b <- b[b$X2 == "wdt:P279",],
   "wdt:P31" = b <- b[b$X2 == "wdt:P31",],
   print("?")
)
#head(b)

if(x == 1){
d <- data.frame(b, X4="down", X5=x, X6=paste0(b$X1, ".", b$X2, ".", b$X3))
readr::write_csv(d, file=paste0("./04_Out/Down3_wd:Q35120_", Proc, "_Depth_", N, ".csv"), append=F, col_names=F)
}else{
d <- data.frame(b, X4="down", X5=x, X6=paste0(b$X1, ".", b$X2, ".", b$X3))
readr::write_csv(d, file=paste0("./04_Out/Down3_wd:Q35120_", Proc, "_Depth_", N, ".csv"), append=T, col_names=F)
if(x == N){break}
}
Query <- b[,1]
}
}

###############################################################
#確認
###############################################################
if(F){
rm(list=ls())

#both
#file_path <- "./04_Out/Down3_wd:Q35120_both.csv"
#wdt:P279 only
#file_path <- "./04_Out/Down3_wd:Q35120_wdt:P279.csv"
#wdt:P31 only
#file_path <- "./04_Out/Down3_wd:Q35120_wdt:P31.csv"

d <- data.frame(readr::read_csv(file_path, col_names = F))
head(d)
table(d$X5)
table(d$X5, d$X2)

length(d$X6)
length(unique(d$X6))
length(unique(unlist(d[,c(1,3)])))

d.i <- d[d$X2 == "wdt:P31",]
if(nrow(d.i) != 0){
head(d.i)
d.ii <- paste0(d.i[,1], "|", d.i[,3])
head(d.ii)  
}else{
d.ii <- NA
}
###############################################################
###############################################################
#起点とのマッティング
Count <- data.frame(readr::read_csv("./04_Out/SearchNum_count02_210318.csv"))
head(Count)
Count$CommonEntity
head(d)
dim(d)
###############################################################
#001
Count00 <- Count[Count$CommonEntity %in% unique(d[d$X5 == 1,]$X1),]
head(Count00)
nrow(Count00)
paste0(Count00$entityNamej, "(", Count00$CommonEntity, ")", collapse = ", ")

#002
Count00 <- Count[Count$CommonEntity %in% unique(d[d$X5 == 2,]$X1),]
head(Count00)
nrow(Count00)
paste0(Count00$entityNamej, "(", Count00$CommonEntity, ")", collapse = ", ")

#003
Count00 <- Count[Count$CommonEntity %in% unique(d[d$X5 == 3,]$X1),]
head(Count00)
nrow(Count00)
paste0(Count00$entityNamej, "(", Count00$CommonEntity, ")", collapse = ", ")

###############################################################
###############################################################
#グラフ作成
###############################################################
library(igraph)

head(d)
N <- 3
dd <- d[c(d$X5 <= N),]
head(dd)

g <- simplify(graph_from_edgelist(as.matrix(dd[,c(1,3)]), directed = T))
#g <- decompose(g)[[1]]

V(g)$color <- "lightyellow"
V(g)$size <- 0.5
V(g)$label.cex <- 0

f <- names(V(g)) %in% dd[c(dd$X5 == 2),c(1)]
V(g)$color[f] <- "skyblue"  

f <- names(V(g)) %in% dd[c(dd$X5 == 3),c(1)]
V(g)$color[f] <- "lightgreen"  
V(g)$size[f] <- 0.25

f <- names(V(g)) %in% "wd:Q35120"
V(g)$color[f] <- "pink"
V(g)$size[f] <- 1

E(g)$width <- 0.1
head(attr(E(g), "vnames"))
E(g)$lty <- 1
E(g)$col <- "grey"
if(!is.na(d.ii)){
E(g)$lty[attr(E(g), "vnames") %in% d.ii] <- 2
E(g)$col[attr(E(g), "vnames") %in% d.ii] <- scales::alpha("red", .3)
}

system.time(layout <- layout_with_fr(g))
str(layout)

Network_plot(g, N=1)

############################################################
Network_plot <- function(g, N=1){
el <- data.table::as.data.table(as_edgelist(g)) %>% 
  data.table::setnames(c("x","y"))
d_layout <- data.table::data.table(x_coord = layout[,1], y_coord = layout[,2], id = names(V(g)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

quartz(width=7.5, height=7.5)
set.seed(123)
par(cex.main=0.5, family="HiraKakuProN-W3")
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA)
segments(x0 = el_w_layout$x1, x1 = el_w_layout$x2,
         y0 = el_w_layout$y1, y1 = el_w_layout$y2,
         col = E(g)$col, lwd=0.3, lty=E(g)$lty)
points(d_layout$x_coord, d_layout$y_coord, 
       pch = 21, col="grey50", lwd=0.25, cex = V(g)$size, bg=V(g)$color)
#text(d_layout$x_coord, d_layout$y_coord, text=d_layout$id, cex=0.1)
#quartz.save(file = paste0("./04_Out/Graph_Entity_", formatC(N, width=4, flag="0"), ".pdf"), type = "pdf"); dev.off()  
}
}

