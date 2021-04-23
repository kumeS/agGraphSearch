rm(list=ls())
getwd()
source("../00_R_Script_gist/SPARQL_AllegroGraph_v07.R")

lab <- read.table("./00_Input/words.lab.csv",
                       sep=",", header=F, fill = TRUE, na.strings = "NA", 
                       check.names = F, stringsAsFactors=F)

if(F){
lab <- read.table("./00_Input/PoLyInfo.lab.csv",
                       sep=",", header=F, fill = TRUE, na.strings = "NA", 
                       check.names = F, stringsAsFactors=F)
}

head(lab)
dim(lab)
length(unique(lab$V1))

#Count並列計算
library(furrr)
plan(multisession(workers = 4))
plan()

#実行
m <- future_map(as.character(unlist(lab)), agCount_Label_Num, .progress = TRUE)
head(fm <- m)

##リストtoデータフレーム
length(fm)
fm1 <- c()
for(n in seq_len(length(fm))){fm1 <- fm1 %>% rbind(fm[[n]])}

head(fm1)
rownames(fm1) <- 1:nrow(fm1)
#table(is.na(fm1))
#table(unlist(fm1[,-1]))

#ラベル
table(fm1$Hit_Label > 0)
table(fm1$Count_As_Label > 0)
table(fm1$Count_As_AltLabel > 0)
table(fm1$Count_As_Label)
table(fm1$Count_As_Label, fm1$Count_As_AltLabel)

#subClassOf
table(fm1$Hit_subClassOf > 0)
table(fm1$Hit_ParentClass > 0)
table(fm1$Hit_ChildClass > 0)

#Hit_Instance
table(fm1$Hit_Instance > 0)
table(fm1$Hit_InstanceOf > 0)
table(fm1$Hit_Has_Instance > 0)

##ラベルあり + subClassOfあり + Instanceあり
fm2 <- fm1[c(fm1$Hit_Label > 0),]
head(fm2)
colnames(fm2)
dim(fm2)

table(fm2$Hit_Label)
table(fm2$Hit_ParentClass_InstanceOf)
table(fm2$Hit_ParentClass)
table(fm2$Hit_InstanceOf)

#Create Relation
fm2$Relation <- fm2$Hit_subClassOf + fm2$Hit_Instance
table(fm2$Hit_subClassOf, fm2$Hit_Instance)
table(fm2$Relation)

#subClassOf, instanceOfどちらかを持つクラス
fm3 <- fm2[c(fm2$Relation > 0),]
head(fm3)
dim(fm3)
table(fm3$Relation > 0)

table(fm3$Hit_Label)
table(fm3$Hit_subClassOf)

###別名を主Class名に変換する
Label_01 <- fm3

head(Label_01)
colnames(Label_01)

table(Label_01$Hit_Label)
table(Label_01$Count_As_Label)
table(Label_01$Count_As_AltLabel)
table(Label_01$Count_As_Label, Label_01$Count_As_AltLabel)

##SPARQL実行
A00 <- Label_01$LABEL
length(A00)

#入力193語彙
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")
A01 <- future_map(unlist(A00), agWD_Alt, .progress = TRUE)
table(names(unlist(map(A01, function(x) table(is.na(x))))))
table(names(unlist(map(A01, function(x) table(nrow(x))))))

#QIDがあるもの
unlist(A00)

head(A01)
A02 <- c()
for(n in seq_len(length(A01))){A02 <- A02 %>% rbind(A01[[n]])}

head(A02)
A02 <- na.omit(A02)
rownames(A02) <- 1:dim(A02)[1]
dim(A02)

##QIDで重複除外
A03 <- A02[as.numeric(rownames(unique(A02["subject"]))),]
length(A03)

QID <- unique(A03)
QID
length(QID)
table(is.na(QID))
#201エンティティ

if(!dir.exists("01_Out")){dir.create("01_Out")}
saveRDS(QID, file = "./01_Out/R01_QID.Rdata", compress = TRUE)

#QIDにLabel情報を付与する
Label00 <- future_map(unlist(QID), agQIDtoLabel, .progress = TRUE)

#出力行のカウント
table(unlist(map(Label00, function(x){nrow(x)})))
#データの結合（list to DF）
Label00a <- c(); for(n in seq_len(length(Label00))){Label00a <- Label00a %>% rbind(Label00[[n]])}

#データサイズ、NAの集計
head(Label00a)
dim(Label00a)
table(is.na(Label00a))
table(is.na(Label00a$entityNamej))
table(is.na(Label00a$entityNamee))

#保存
saveRDS(Label00a, file = "./01_Out/R01_QID_rdfsLabel.Rdata", compress = TRUE)

####################################################
####################################################
#実行の再確認
####################################################
rm(list=ls())
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")
Labs <- readRDS("./01_Out/R01_QID_rdfsLabel.Rdata")
#Labs0 <- readRDS("../01_2_Onto_test_PolyInfo_Ver1.1/01_Out/R01_QID_rdfsLabel_210129.Rdata")
head(Labs)
#head(Labs0)
#Labs0[!c(Labs0$QID %in% Labs$QID),]

#Count並列計算
library(furrr)
plan(multisession(workers = 4))
plan()

lab_2nd <- Labs$QID 
m_2 <- future_map(unlist(lab_2nd), agCount_QID_Num, .progress = TRUE)
head(fm_2 <- m_2)

##リストtoデータフレーム
length(fm_2)
fm_3 <- c()
for(n in seq_len(length(fm_2))){fm_3 <- fm_3 %>% rbind(fm_2[[n]])}

head(fm_3)
dim(fm_3)
colnames(fm_3)

#全体
table(fm_3$Count_Of_P279_P31)
table(fm_3$Count_Of_P279_P31 > 0)
table(fm_3$Count_Of_P279 > 0)
table(fm_3$Count_Of_P31 > 0)

#上位
table(fm_3$Count_Of_P279_P31_up > 0)
table(fm_3$Count_Of_ParentClass > 0)
table(fm_3$Count_InstanceOf > 0)

#下位
table(fm_3$Count_Of_P279_P31_down > 0)
table(fm_3$Count_Of_ChildClass > 0)
table(fm_3$Count_Has_Instance > 0)

############################
#ラベルの再確認
############################
head(Labs)
dim(Labs)

B01 <- future_map(unlist(Labs$entityNamej), agCount_Label_Num, .progress = TRUE)
B02 <- unlist(map(B01, function(x){nrow(x)}))
B02; length(B02)
B02 <- unlist(map(B01, function(x){x$Hit_subClassOf_Instance}))
B02; length(B02)
