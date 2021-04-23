rm(list=ls())
if(!file.exists("./10_Out")){dir.create("./10_Out")}
#if(file.exists("./10_Out/OCR_labels_df.csv")){file.remove("./10_Out/OCR_labels_df.csv")}
#install.packages("stringdist")
library(stringdist)

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

##########################################################################################
##########################################################################################
#対象ラベル情報
##########################################################################################
#ターミナル実行
#cat ./Label_df_rdfslabel.csv | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./Label_df_rdfslabel_nkf.csv; head ./Label_df_rdfslabel_nkf.csv
#cat ./Label_df_skosaltLabel.csv | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./Label_df_skosaltLabel_nkf.csv; head ./Label_df_skosaltLabel_nkf.csv
#cat Label_df_rdfslabel_nkf.csv Label_df_skosaltLabel_nkf.csv > Label_df_rdfslabel_skosaltLabel_nkf.csv
##########################################################################################
if(F){
#完全一致 & 曖昧判定
file_path <- "./wikidata201013_v01_copy/Label_df_rdfslabel_skosaltLabel_nkf.csv"
con_file <- file(description = file_path, open = "r")
con_file
#close(con_file)

x <- 0
N <- 100000

while( TRUE ){
x <- x + 1
print(paste0("No: ", x, " Line: ", x*N ))
try(a <- readLines(con_file, n = N, encoding="UTF-8"), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

#head(a)
a1 <- unlist(strsplit(sub(",", "AiBiCiDiEiFiG", sub(",", "AiBiCiDiEiFiG", a)), split="AiBiCiDiEiFiG"))
if( (length(a1) %% 3) == 0 ){
  b <- matrix(a1, ncol=3, byrow=T)
}else{
  b <- matrix(a1[1:(length(a1) - (length(a1) %% 3))], ncol=3, byrow=T)
}

#head(b)
b[,1] <- sub("^\"", "", b[,1])
b[,1] <- sub("\"$", "", b[,1])
b[,2] <- sub("^\"", "", b[,2])
b[,2] <- sub("\"$", "", b[,2])

b[,3] <- sub("^\"\\\\\"", "", b[,3])
b[,3] <- sub("\"$", "", b[,3])
lang <- stringr::str_sub(b[,3], start=-3, end=-1)
b[,3] <- sub("\\\\\"@ja$", "", b[,3])
b[,3] <- sub("\\\\\"@en$", "", b[,3])
#head(b, n =20)
b1 <- data.frame(b)
b1$X4 <- lang
#head(b1, n =20)

#JPN
cc <- b1$X3 %in% OCR_jpn
d <- b1[cc,]

#Dat <- matrix(NA, nrow=length(b1$X3), ncol=length(OCR_jpn))
#for(n in 1:length(OCR_jpn)){
#print(n); Dat[,n] <- agrepl(OCR_jpn[n], b1$X3)
#}
#apply(Dat, 1, any)

if(nrow(d) > 0){
readr::write_csv(d, file=paste0("./10_Out/OCR_labels_df.csv"), append=T, col_names=F)  
}
}

file.copy(from="./10_Out/OCR_labels_df.csv", to="./10_Out/OCR_labels_df_copy.csv")
}
##########################################################################################
#一致度評価
##########################################################################################
############################################################
#全体
############################################################
if(F){
library(magrittr)
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02_label_nkf.csv"), col_names = F))
head(A); dim(A)

#NA確認
table(is.na(A$X1))
table(is.na(A$X2))
A[is.na(A$X2),]
table(is.na(A$X3))
table(is.na(A$X4))
#A <- data.frame(na.omit(A))
#str(A)
#wd:
table(grepl("wd:", A$X1))
#ja/en
table(A$X3)
A$X3[A$X3 == ";en"] <- "en"
A$X3[A$X3 == "en;"] <- "en"
table(A$X3)
#Label
table(A$X4)
#文字長さNA
table(is.na(nchar(A$X2)))
head(A)



############################################################
#All
############################################################
a <- A
length(unique(a$X1))
length(a$X2)
length(unique(a$X2))
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])
#length(unique(a$X2)[unique(a$X2) %in% OCR_en])
#length(unique(a$X2)[unique(a$X2) %in% OCR_all])
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])/length(unique(a$X2))

#JPN
a <- A[A$X3 == "ja",]
length(unique(a$X1))
length(a$X2)
length(unique(a$X2))
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])
#length(unique(a$X2)[unique(a$X2) %in% OCR_en])
#length(unique(a$X2)[unique(a$X2) %in% OCR_all])
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])/length(unique(a$X2))

#JPN/rdfs:label
head(A)
a <- A[A$X3 == "ja",]
a <- a[a$X4 == "rdfs:label",]
length(unique(a$X1))
length(a$X2)
length(unique(a$X2))
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])
#length(unique(a$X2)[unique(a$X2) %in% OCR_en])
#length(unique(a$X2)[unique(a$X2) %in% OCR_all])
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])/length(unique(a$X2))

#JPN/rdfs:label
head(A)
a <- A[A$X3 == "ja",]
a <- a[a$X4 == "rdfs:label",]
length(unique(a$X1))
length(a$X2)
length(unique(a$X2))
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])
#length(unique(a$X2)[unique(a$X2) %in% OCR_en])
#length(unique(a$X2)[unique(a$X2) %in% OCR_all])
length(unique(a$X2)[unique(a$X2) %in% OCR_jpn])/length(unique(a$X2))

#リスト
SearchNum00 <- readRDS("./04_Out/SearchNum00_210204.Rdata")
SearchLabel <- SearchNum00[!is.na(SearchNum00$Levels),]
SearchLabel <- SearchLabel[order(SearchLabel$Levels),]
SearchLabel$entityNamej[is.na(SearchLabel$entityNamej)] <- SearchLabel$entityNamee[is.na(SearchLabel$entityNamej)]
SearchLabel <- SearchLabel[SearchLabel$Levels <= 5,]
rownames(SearchLabel) <- 1:nrow(SearchLabel)
head(SearchLabel)

DD <- c()
for(k in 1:nrow(SearchLabel)){
#k <- 1
d <- paste0("./08_Out/", formatC(k, width=5, flag="0"), "_", SearchLabel$CommonEntity[k], "_uni_cut.csv")
D <- data.frame(readr::read_csv(d, col_names = F))
head(D)
DD <- DD %>% rbind(D)
}  

head(A)
length(unique(A$X1))
length(unique(A$X2))

a <- A[A$X3 == "ja",]
head(a)
dim(a)
length(unique(a$X1))
length(unique(a$X2))

head(DD)
DD00 <- unique(c(DD$X1, DD$X3))
length(DD00)
head(DD00)

aa <- a[c(a$X1 %in% DD00),]
head(aa)
dim(aa)
length(unique(aa$X1))
length(unique(aa$X2))

length(unique(aa$X2)[unique(aa$X2) %in% OCR_jpn])
#length(unique(a$X2)[unique(a$X2) %in% OCR_en])
#length(unique(a$X2)[unique(a$X2) %in% OCR_all])
length(unique(aa$X2)[unique(aa$X2) %in% OCR_jpn])/length(unique(aa$X2))

1144/1144
1144/366179
883/1144
883/235869
985/1144
985/195533
}

############################################################
#個別 & サブツリーごと
############################################################
#リスト
SearchNum00 <- readRDS("./04_Out/SearchNum00_210204.Rdata")
SearchLabel <- SearchNum00[!is.na(SearchNum00$Levels),]
SearchLabel <- SearchLabel[order(SearchLabel$Levels),]
SearchLabel$entityNamej[is.na(SearchLabel$entityNamej)] <- SearchLabel$entityNamee[is.na(SearchLabel$entityNamej)]
SearchLabel <- SearchLabel[SearchLabel$Levels <= 5,]
rownames(SearchLabel) <- 1:nrow(SearchLabel)
head(SearchLabel)

Dat <- data.frame(matrix(NA, nrow=nrow(SearchLabel), ncol=13))
Dat$X1 <- SearchLabel$CommonEntity
Dat$X2 <- SearchLabel$entityNamej
Dat$X3 <- SearchLabel$Levels

for(k in 1:nrow(SearchLabel)){
#k <- 1
print(k)
b <- paste0("./05_Out_txt/", formatC(k, width=5, flag="0"), "_", SearchLabel$CommonEntity[k], "_label.csv")
B <- data.frame(readr::read_csv(b, col_names = F))

head(B)
#完全一致
BB <- B[B$X3 == "ja",]
ff <- unique(BB$X2); ff <- ff[!is.na(ff)]
Dat[k,4] <- length(ff)
Dat[k,5] <- length(ff[ff %in% OCR_jpn])
Dat[k,6] <- round(Dat[k,5]/Dat[k,4], 6)

#曖昧一致
if(length(ff) < 1000){
cc <- length(ff[stringdist::ain(ff, OCR_jpn, method ="osa", maxDist = 0.3)])
Dat[k,7] <- cc
Dat[k,8] <- round(cc/Dat[k,4], 6)
}

#方向を考慮
d <- paste0("./08_Out/", formatC(k, width=5, flag="0"), "_", SearchLabel$CommonEntity[k], "_uni_cut.csv")
D <- data.frame(readr::read_csv(d, col_names = F))
head(D)
BB00 <- BB[c(BB$X1 %in% unique(c(D$X1, D$X3))),]
head(BB00)
ff <- unique(BB00$X2); ff <- ff[!is.na(ff)]
Dat[k,9] <- length(ff)
Dat[k,10] <- length(ff[ff %in% OCR_jpn])
Dat[k,11] <- round(Dat[k,10]/Dat[k,9], 6)

Dat[k,12] <-  ifelse(Dat[k,11] > Dat[k,6], "補正OK", "")
Dat[k,13] <-  round(Dat[k,9]/Dat[k,4], 3)

colnames(Dat) <- c("QID", "Label", "Levels",
  "Count", "Count_in", "Count_per",
  "Count_ain", "Count_ain_per",
  "Count_h", "Count_h_in", "Count_h_per", "HIGH", "Ratio")
readr::write_csv(Dat, file="./10_Out/Count_kiten.csv", append=F, col_names = T)  
}

##############################
##Wikidataとの一致度
##############################
source("../00_R_Script_gist/SPARQL_wikiWD_01.R")

#実行テスト
#wikiCount_Label_Num(EntityName="α-fetoprotein", FilterRegex=F)

#Count並列計算
library(furrr)
plan(multisession(workers = availableCores())); plan()

OCR
future_map(unlist(OCR), wikiCount_Label_Num, .progress = TRUE)

#エラー出たら繰り返し実行
DIR <- dir("./R12_Results")
DIR00 <- gsub(".Rdata", "", DIR)
OCR00 <- OCR[!c(OCR %in% DIR00)]
OCR00
future_map(unlist(OCR00), wikiCount_Label_Num, .progress = TRUE)

##読み込み
D <- c()
for(n in seq_len(length(DIR))){
#n <- 1
E <- readRDS(paste("./R12_Results/", DIR[n], sep=""))
D <- D %>% rbind(E)
}
head(D)
table(D$Hit_Label)
LAB00 <- D[D$Hit_Label > 0,]
table(LAB00$Hit_Label)

#Wikidata一致との一致度
length(c(LAB00$LABEL))
length(c(LAB00$LABEL %in% A)[c(LAB00$LABEL %in% A) == TRUE])

#1008/1987*100

#C <- c()
#for(n in 1:length(OCR)){
#n <- 1
#Dat00 <- wikiCount_Label_Num(EntityName=unlist(OCR)[n])
#C <- C %>% rbind(Dat00)
#}

head(C)

##############################
##Wikidataとの曖昧検索
##############################
#dir.create("R12_Results_A")
wikiCount_AmbiguousLabel_Num(EntityName="α-fetoprotein", lang=1, Message=T)

#Count並列計算
library(furrr)
plan(multisession(workers = availableCores())); plan()



OCR
Dat01 <- future_map(unlist(OCR), wikiCount_AmbiguousLabel_Num, .progress = TRUE)


