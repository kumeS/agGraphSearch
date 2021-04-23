rm(list=ls())
library(magrittr)
if(!file.exists("./12_Out")){dir.create("./12_Out")}
########################################################################
########################################################################
#高分子オントロジーのクラス階層の読み込み
file_path <- "./05_Out_txt/All_uni_02.csv"
a <- data.frame(readr::read_csv(file_path, col_names = F))
head(a)

#下位取得&出力
SubSearch <- function(Mate=Mate, Intermdedi=Intermdedi){
for(n in seq_len(length(Mate))){
#n <- 2
#下位検索
message(paste0("ID: ", Mate[n]))
Query <- Mate[n]
x <- 0

repeat{
x <- x +1
message(paste0("down: ", x))
cc <- c(a[,3] %in% Query)
sum(cc)
b <- a[cc,]
#head(b)

if(nrow(b) > 0){
b <- data.frame(b[,1:3], X4="down", X5=x, X6=Mate[n], X7=paste0(b$X1, ".", b$X2, ".", b$X3))
if(x == 1){
readr::write_csv(b, file=paste0("./12_Out/", Intermdedi, "_", Mate[n], ".csv"), append=F, col_names = F )  
}else{
readr::write_csv(b, file=paste0("./12_Out/", Intermdedi, "_", Mate[n], ".csv"), append=T, col_names = F )
}
}else{
break  
}
Query <- unique(b[,1])
}
#retry
abc <- data.frame(readr::read_csv(file=paste0("./12_Out/", Intermdedi, "_", Mate[n], ".csv"), col_names = F ))
#head(abc)
abc0 <- abc[as.numeric(rownames(unique(abc["X7"]))),]
readr::write_csv(abc0, file=paste0("./12_Out/", Intermdedi, "_", Mate[n], "_uni.csv"), append=F, col_names = F )  
}}

SubBind <- function(Mate, Intermdedi){
Dat <- c()
for(n in seq_len(length(Mate))){
#n <- 1
message(paste0("No.: ", n, " ID: ", Mate[n]))
abc <- data.frame(readr::read_csv(file=paste0("./12_Out/", Intermdedi, "_", Mate[n], "_uni.csv"), col_names = F ))
Dat <- Dat %>% rbind(abc)
}
#dim(Dat)
rownames(Dat) <- 1:nrow(Dat)
Dat0 <- Dat[as.numeric(rownames(unique(Dat["X7"]))),]
#dim(Dat0)
readr::write_csv(Dat0, file=paste0("./12_Out/", Intermdedi, "_Dat.csv"), append=F, col_names = F )  
}
########################################################################
########################################################################
#マテリアル
Intermdedi0 <- "Material"
#探索するIDリスト決める
Mate0 <- c("wd:Q79529", "wd:Q1310239", "wd:Q28732711", "wd:Q30308136", "wd:Q17339814", "wd:Q1704572")
SubSearch(Mate=Mate0, Intermdedi=Intermdedi0)
SubBind(Mate=Mate0, Intermdedi=Intermdedi0)

#物性
Intermdedi0 <- "Characteristics"
#探索するIDリスト決める
Chara0 <- c("wd:Q47574", "wd:Q309314", "wd:Q16722960", "wd:Q4373292", "wd:Q614112", "wd:Q3523867", "wd:Q3505845", "wd:Q71550118")
SubSearch(Mate=Chara0, Intermdedi=Intermdedi0)
SubBind(Mate=Chara0, Intermdedi=Intermdedi0)
  
#プロセス
Intermdedi0 <- "Process"
#探索するIDリスト決める
Pro0 <- c("wd:Q20937557", "wd:Q1799072", "wd:Q39546")
SubSearch(Mate=Pro0, Intermdedi=Intermdedi0)
SubBind(Mate=Pro0, Intermdedi=Intermdedi0)

########################################################################
########################################################################
#ラベル取得 to csv
########################################################################
rm(list=ls())
file_path <- "./05_Out_txt/All_uni_02_label_nkf.csv"
b <- data.frame(readr::read_csv(file_path, col_names = F))
b <- b[b$X3 =="ja", ]

head(b)
table(b$X4)
table(b$X3)
#b[b$X3 == ";en",]$X3 <- "en"; b[b$X3 == "en;",]$X3 <- "en"
table(b$X3)

######################
#文字数
b$X5 <- nchar(b$X2)
dim(b)
b <- b[b$X5 != 1,]
dim(b)
b <- b[b$X5 < 20,]
dim(b)

head(b$X2)
b <- b[!grepl("[0-9][.][0-9]", b$X2),]
b <- b[!grepl("^[0-9]$", b$X2),]
b <- b[!grepl("^[0-9][0-9]$", b$X2),]
b <- b[!grepl("^[0-9][0-9][0-9]$", b$X2),]
b <- b[!grepl("[0-9][0-9][0-9][0-9]", b$X2),]
b <- b[!grepl("[0-9][0-9][0-9][0-9][0-9]", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]$", b$X2),]
b <- b[!grepl("^[.][[:alnum:]]", b$X2),]
b <- b[!grepl("[〜]", b$X2),]
b <- b[!grepl("[〜]", b$X2),]
b <- b[!grepl("^[・]", b$X2),]
b <- b[!grepl("映画", b$X2),]
dim(b)

######################
Intermdedi00 <- c("Material", "Characteristics", "Process")
Intermdedi01 <- c("マテリアル", "物性", "プロセス")

for(n in 1:length(Intermdedi00)){
#n <- 1
d <- data.frame(readr::read_csv(file=paste0("./12_Out/", Intermdedi00[n], "_Dat.csv"), col_names = F ))
#head(d)
list <- unique(c(d$X1, d$X3))
cc <- b$X1 %in% list
bb <- b[cc,]
#head(bb)
readr::write_csv(bb, file=paste0("./12_Out/", Intermdedi00[n], "_Dat_Labels.csv"), append=F, col_names = F )  
e <- data.frame(X=bb$X2, Y=Intermdedi01[n])
#head(e); dim(e)
e0 <- e[as.numeric(rownames(unique(e["X"]))),]
#head(e0); dim(e0)
readr::write_excel_csv(e0, file=paste0("./12_Out/", Intermdedi00[n], "_Dat_TermDict.csv"), append=F, col_names = F )  
}

#############################
##TermDictの結合
#############################
rm(list=ls())
library(magrittr)
TermDict <- c()
Intermdedi00 <- c("Material", "Characteristics", "Process")
for(n in 1:length(Intermdedi00)){
#n <-1
ff <- data.frame(readr::read_csv(file=paste0("./12_Out/", Intermdedi00[n], "_Dat_TermDict.csv"), col_names = F ))
TermDict <- TermDict %>% rbind(ff)
}

head(TermDict)
TermDict00 <- tidyr::spread(TermDict, key=X2, value=X2)
head(TermDict00)
TermDict00[is.na(TermDict00)] <- ""
#TermDict00 <- TermDict00[order(TermDict00$X1, decreasing = F),]
head(TermDict00)
#head(t(apply(TermDict00[,-1], 1, function(x) x[order(x, decreasing = T)])))
TermDict00[,-1] <- t(apply(TermDict00[,-1], 1, function(x) x[order(x, decreasing = T)]))
readr::write_excel_csv(TermDict00, file=paste0("./12_Out/Intermdediates_TermDict.csv"), append=F, col_names = F )  






