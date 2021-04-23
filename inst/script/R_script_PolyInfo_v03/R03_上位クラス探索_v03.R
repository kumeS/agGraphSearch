rm(list=ls())

source("../00_R_Script_gist/SPARQL_AllergroGraph_PropertyPath_v04.R")
source("../00_R_Script_gist/SPARQL_AllergroGraph_VisNet_v01.R")
source("../00_R_Script_gist/Text_proc_v01.R")

QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")

head(QID_rdfsLabel02)
dim(QID_rdfsLabel02)
colnames(QID_rdfsLabel02)

QID <- unique(QID_rdfsLabel02[,1])
rdfsLabel <- unique(QID_rdfsLabel02[,2])

#######################################################
#すべてのラベルの上位クラスのグラフを結合する
#IDで検索
#######################################################
#if(T){}
ID <- unlist(unique(QID))
length(ID)

##SPARQL実行
#availableCores()
plan(multisession(workers = 4))
plan()

#SPARQL確認
wikiGraph_01 <- future_map(unlist(ID), AllergroPropertyPath_Graph_up_v2, .progress = TRUE)

#Text
#file_path <- "./wikidata201013_v01_copy/Relation_P31_P279_df.csv"
#a <- data.frame(readr::read_csv(file_path, col_names = F))
#rdfGraph_01 <- Text_proc_up(List=QID_rdfsLabel02, Data=a)

##カウント
head(wikiGraph_01)
wikiGraph_01[[1]]

#取得データ行のカウント
B01 <- unlist(map(wikiGraph_01, function(x){nrow(x)}))
B01; sum(B01); length(B01); table(B01)

##ループの除外
head(wikiGraph_01[[1]])
map(wikiGraph_01, function(x){table(x[,3] == x[,5])})
B02 <- unlist(map(wikiGraph_01, function(x){table(x[,3] == x[,5])}))
B02; length(B02); sum(B02[names(B02) == "TRUE"])

head(wikiGraph_01[[1]])
if(F){
for(n in seq_len(length(wikiGraph_01))){
wikiGraph_01[[n]] <- wikiGraph_01[[n]][wikiGraph_01[[n]]$subject != wikiGraph_01[[n]]$parentClass,]
}}

#保存
head(wikiGraph_01)
if(!dir.exists("03_Out")){dir.create("03_Out")}
saveRDS(wikiGraph_01, file = "./03_Out/R03_wikiGraph_01_up.Rdata", compress = TRUE)

#個別ネットワーク図の保存
if(!dir.exists("03_Out_vis")){dir.create("03_Out_vis")}

for(n in 1:length(wikiGraph_01)){
#n <- 1
Lab <- QID_rdfsLabel02[n,1:2]; Lab00 <- paste(Lab[c(2, 1)], collapse = ".")
agVisNetwork(Graph=wikiGraph_01[[n]], 
             NodeColorRandom=F, Count=2, 
             Size=10, SmallSize=5, StarSize=10, 
             FontSize=7, HeightSclale = "750px", 
             WidthSclale = "110%", SEED=123, Selected=Lab00, 
             Browse=F, output=T, file=paste0("./agVisNetwork_", formatC(n, flag="0", width=4), "_", Lab00, ".html"))
system(paste0("mv -f './agVisNetwork_", formatC(n, flag="0", width=4), "_", Lab00, ".html' ./03_Out_vis"))
#agNetwork3d(Graph=wikiGraph_01[[n]])
if(dir.exists(paste0("./agVisNetwork_", formatC(n, flag="0", width=4), "_", Lab00, "_files"))){
  system(paste0('rm -rf "./agVisNetwork_', formatC(n, flag="0", width=4), '_', Lab00, '_files"'))
}}

################################################################
################################################################
#wikiGraph_01: 個別グラフ
rm(list=ls())
wikiGraph_01 <- readRDS(file = "./03_Out/R03_wikiGraph_01_up.Rdata")

##リストtoデータフレーム
head(wikiGraph_01)
head(wikiGraph_01[[1]])
wikiGraph_01[[1]]
length(wikiGraph_01)

#そのまま追加
wikiGraph_02 <- c()
for(n in seq_len(length(wikiGraph_01))){
  wikiGraph_02 <- wikiGraph_02 %>% rbind(wikiGraph_01[[n]])
}
dim(wikiGraph_02)
rownames(wikiGraph_02) <- 1:nrow(wikiGraph_02)

##NAの確認
table(is.na(wikiGraph_02))
table(is.na(wikiGraph_02$subjectLabel))
table(is.na(wikiGraph_02$parentClassLabel))

#wikiGraph_02: 結合グラフ
dim(wikiGraph_02)
head(wikiGraph_02)

#ループを除外して、追加
if(T){
wikiGraph_02L <- c()
for(n in seq_len(length(wikiGraph_01))){
  a <- wikiGraph_01[[n]]
  b <- a[c(a$subject != a$parentClass),]
  wikiGraph_02L <- wikiGraph_02L %>% rbind(b)
}}

dim(wikiGraph_02)
dim(wikiGraph_02L)

#確認
head(wikiGraph_02)
rownames(wikiGraph_02)
rownames(wikiGraph_02) <- 1:dim(wikiGraph_02)[1]
dim(wikiGraph_02)

saveRDS(wikiGraph_02,
        file="./03_Out/R03_wikiGraph_02_up.Rdata",
        compress = TRUE)
##############################################################################
##############################################################################
##############################################################################
rm(list=ls())
wikiGraph_01 <- readRDS(file = "./03_Out/R03_wikiGraph_01_up.Rdata")
wikiGraph_02 <- readRDS(file = "./03_Out/R03_wikiGraph_02_up.Rdata")

##重複無しトリプル数
head(wikiGraph_02)
#エンティティ数
E01 <- length(unique(c(wikiGraph_02$subject, wikiGraph_02$parentClass)))
E01
#ラベル数
E02 <- length(unique(c(wikiGraph_02$subjectLabel, wikiGraph_02$parentClassLabel)))
E02
#トリプル数
E03 <- length(unique(wikiGraph_02$unique))
E03

###共通エンティティの計算
head(wikiGraph_01)
dim(wikiGraph_01)

B02 <- unlist(map(wikiGraph_01, function(x){unique(x$parentClass)}))
table(B02)
unique(B02)
length(unique(B02))

##Count 01
pc_table <- table(B02)
head(pc_table)
table(pc_table)
pc_table1 <- data.frame(parentClass=names(pc_table),
                                Freq=as.numeric(pc_table), 
                                 row.names = 1:length(pc_table),
                                stringsAsFactors = F)
head(pc_table1)

####################################################################
####################################################################
head(wikiGraph_02)
#parentClass parentClassLabelをとる
Dat <- data.frame(wikiGraph_02[,c(colnames(wikiGraph_02) == "parentClass" | colnames(wikiGraph_02) == "parentClassLabel")], stringsAsFactors = F)
head(Dat)
dim(Dat)
dim(pc_table1)

head(pc_table1)
Dat2 <- Dat[as.numeric(rownames(unique(Dat["parentClass"]))),]
head(Dat2)

#マージ
pc_table2 <- merge(pc_table1, Dat2, by="parentClass",
                            all = T, sort = F)
head(pc_table2)
dim(pc_table2)

pc_table2.o <- pc_table2[order(pc_table2$Freq),]
head(pc_table2.o)
pc_table2.order2 <- pc_table2.o[pc_table2.o$Freq > 1,]

head(pc_table2.order2)
table(pc_table2.order2$Freq)
dim(pc_table2.order2)
length(unique(pc_table2.order2$parentClass))

#保存
saveRDS(pc_table2.order2, file = "./03_Out/R03_pc_table2.Rdata", compress = TRUE)
readr::write_excel_csv(pc_table2.order2, file=paste("./03_Out/R03_pc_table2.csv", sep=""))

#######################
##包含率の計算
#######################
#rm(list=ls())
QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")
QID <- unique(QID_rdfsLabel02[,1])
rdfsLabel <- unique(QID_rdfsLabel02[,2])

wikiGraph_01 <- readRDS(file = "./03_Out/R03_wikiGraph_01_up.Rdata")
wikiGraph_02 <- readRDS(file = "./03_Out/R03_wikiGraph_02_up.Rdata")

head(wikiGraph_02)

##QID
qid <- unique(wikiGraph_02$subject, wikiGraph_02$parentClass)
b <- setdiff(QID, qid)
b; length(b)

##rdfsLabel
RdfsLabel <- unique(wikiGraph_02$subjectLabel, wikiGraph_02$parentClassLabel)
b <- setdiff(rdfsLabel, RdfsLabel)
b; length(b)

######################################################################################
######################################################################################
##可視化
######################################################################################
if(T){
source("../00_R_Script_gist/SPARQL_AllergroGraph_VisNet_v01.R")
  
head(wikiGraph_02)
wikiGraph_02LL <- wikiGraph_02
head(wikiGraph_02LL)

##グラフ化
NAME <- paste("R03_wikiGraph_02_全ラベル_", "エンティティ数", E01, "_ラベル数", E02, "_トリプル数", E03, "_", sep="")
file_name <- paste0(NAME, format(Sys.time(), "%y%m%d_%H%M"),".html")
GraphPlot <- agVisNetwork(Graph=wikiGraph_02LL, NodeColorRandom=T,Count=4, Size=10, FontSize=7, SEED=123, 
               HeightSclale = "750px", Selected=NULL, Browse=T, output=T,
               file=file_name, outputNodesEdges=F)
GraphPlot
system(paste0("mv -f '", file_name, "' ./03_Out_vis"))

if(dir.exists(paste0('rm -rf ', sub(".html", "", file_name), "_files"))){
system(paste0('rm -rf ', sub(".html", "", file_name), "_files"))  
}

#エンティティ数
length(unique(c(wikiGraph_02LL$subject, wikiGraph_02LL$parentClass)))
#ラベル数
length(unique(c(wikiGraph_02LL$subjectLabel, wikiGraph_02LL$parentClassLabel)))
}

######################################################################################
######################################################################################
if(T){
library(plotly)
pc_table2.order2 <- readRDS(file ="./03_Out/R03_pc_table2.Rdata")

MMMM <- "P31_P298_１回目のみ_"
head(pc_table2.order2)
pc_table2.order2 <- pc_table2.order2[order(-pc_table2.order2$Freq),]
Data <- data.frame(V1=pc_table2.order2$parentClassLabel, 
                    V2=as.numeric(pc_table2.order2$Freq),
                   V3=1:nrow(pc_table2.order2))
fig <- plot_ly(Data, x = ~V3, y = ~V2, type = 'bar', name = '',
               hovertemplate = paste(Data$V1, '<br>%{y}<br>', sep=""),
               marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% layout(title = paste("Frequency as a common class ", sep=""),
                      yaxis = list(title = 'Count'),
                      xaxis = list(title = ""),
                      xaxis = Data$V1)
fig
#保存
if(!dir.exists("03_Out_ly")){dir.create("03_Out_ly")}
Nam <- paste0("R03_全体ラベル_共通クラスの頻度グラフ_回数_", MMMM, format(Sys.time(), "%y%m%d_%H%M"),"_R.html")
htmlwidgets::saveWidget(plotly::as_widget(fig), Nam )
system(paste0("mv -f '", Nam, "' ./03_Out_ly"))

#ラベル数で補正
length(QID)
Data <- data.frame(V1=pc_table2.order2$parentClassLabel, 
                   V2=round(as.numeric(pc_table2.order2$Freq)/length(QID)*100, 2),
                   V3=1:nrow(pc_table2.order2))
fig <- plot_ly(Data, x = ~V3, y = ~V2, type = 'bar', name = '',
               hovertemplate = paste(Data$V1, '<br>%{y}<br>', sep=""),
               marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% layout(title = paste("Percent (%) as a common class ", sep=""),
                      xaxis = list(title = ""),
                      yaxis = list(title = 'Percent', range = c(0, 100)))
fig

Nam <- paste("./R03_全体ラベル_共通クラスの頻度グラフ_Percent_", MMMM, format(Sys.time(), "%y%m%d_%H%M"),"_R.html", sep="")
htmlwidgets::saveWidget(plotly::as_widget(fig), Nam )
system(paste0("mv -f '", Nam, "' ./03_Out_ly"))
}


  
