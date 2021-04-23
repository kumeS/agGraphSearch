#ファイル全体
rm(list=ls())

A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02.csv"), col_names = F))
head(A)
dim(A)

a <- data.frame(matrix(NA, ncol=5, nrow=range(A$X7)[2]))
colnames(a) <- c("No", "each_triple", "each_entity", 
                 "all_triple", "all_entity")
head(a)


for(n in 1:range(A$X7)[2]){
#n <- 1
#head(A)
print(n)
B <- A[A$X7 == n,]
C <- B[B$X4 == "down",]
#head(C)
a[n,1] <- n
a[n,2] <- length(unique(C$X8))
a[n,3] <- length(unique(c(C$X1, C$X3)))

B <- A[A$X7 <= n,]
C <- B[B$X4 == "down",]
a[n,4] <- length(unique(C$X8))
a[n,5] <- length(unique(c(C$X1, C$X3)))

#head(a)
}

a

################################################################################
################################################################################

if(F){

if(!file.exists("./07_Out")){dir.create("./07_Out")}
library(plotly)

head(a)
fig <- plot_ly(a, x = ~No, y = ~each_entity, type = 'bar', name = '',
               hovertemplate = paste('%{y}', sep=""),
               marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% layout(title = paste("展開段数 v.s. 下位概念のエンティティ数（QID数)", sep=""),
                      yaxis = list(title = 'QID数のカウント', linewidth = 1),
                      xaxis = list(title = "展開段数", ticks = "outside", linewidth = 0,
                                     tick0 = 1, dtick = 1),
                      margin = list(l=50, r=10, b=50, t=50, pad=4))
fig

Filename <- paste0("./Barplot_results_", format(Sys.time(), "%y%m%d"),"_01.html")
htmlwidgets::saveWidget(plotly::as_widget(fig), file=Filename)
system(paste0("mv ", Filename, " ./07_Out"))

head(a)
fig <- plot_ly(a, x = ~No, y = ~all_entity, type = 'bar', name = '',
               hovertemplate = paste('%{y}', sep=""),
               marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 0.25)))
fig <- fig %>% layout(title = paste("展開段数 v.s. 下位概念のエンティティ数（のべQID数)", sep=""),
                      yaxis = list(title = 'QID数のカウント', linewidth = 1),
                      xaxis = list(title = "展開段数", ticks = "outside", linewidth = 0,
                                     tick0 = 1, dtick = 1),
                      margin = list(l=50, r=10, b=50, t=50, pad=4))
fig
Filename <- paste0("./Barplot_results_", format(Sys.time(), "%y%m%d"),"_02.html")
htmlwidgets::saveWidget(plotly::as_widget(fig), file=Filename)
system(paste0("mv ", Filename, " ./07_Out"))
}

################################################################################
#Unicodeエスケープの変換
#system("which nkf")
#system("brew install nkf")
#cd /Users/sas/Desktop/ToDO_210205/01_2_Onto_test_PolyInfo_Ver1.1/05_Out_txt
#cat ./All_uni_02_label.nt | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./All_uni_02_label_nkf.nt; head ./All_uni_02_label_nkf.nt
#cat ./All_uni_02_label.csv | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./All_uni_02_label_nkf.csv; head ./All_uni_02_label_nkf.csv
################################################################################
rm(list=ls())

library(magrittr)
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02_label_nkf.csv"), col_names = F))
head(A)
dim(A)

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

################################################################################
##起点ごとでLabelを抽出して、書き出す
################################################################################
head(A)

D00 <- "./05_Out_txt"
SearchNum00 <- readRDS("./04_Out/SearchNum00_210204.Rdata")
SearchLabel <- SearchNum00[!is.na(SearchNum00$Levels),]
SearchLabel <- SearchLabel[order(SearchLabel$Levels),]
SearchLabel <- SearchLabel[SearchLabel$Levels <= 5,]
rownames(SearchLabel) <- 1:nrow(SearchLabel)

dim(SearchLabel)
head(SearchLabel)
tail(SearchLabel)
#saveRDS(SearchLabel, file = "./07_Out/SearchNum.Rdata")
#readr::write_excel_csv(SearchLabel, file = "./07_Out/SearchNum.csv", col_names=F)

for(n in 1:nrow(SearchLabel)){
#n <- 1
print(n)
a <- paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", SearchLabel$CommonEntity[n], ".csv")
B <- data.frame(readr::read_csv(a, col_names = F))

#head(B)
b <- unique(c(B$X1, B$X3))

c1 <- A$X1 %in% b 
c2 <- A[c1, ]

#head(c2)
readr::write_csv(c2, file=paste0(sub(".csv$", "", a), "_label.csv"), 
  append=F, col_names = F)

}

################################################################################
##community解析の結果と紐付け
################################################################################
rm(list=ls())

SearchLabel <- readRDS("./07_Out/SearchNum.Rdata")
head(SearchLabel)

a <- data.frame(readr::read_csv(paste0("./06_Out_txt/Comm.csv"), col_names = F))
head(a)
colnames(a) <- c("Name.QID", "Name", "QID", "CommVal")

b <- merge(SearchLabel, a, by="QID", all = T, sort = F)
head(b)
table(b$CommVal, useNA = "always")
b$CommVal[is.na(b$CommVal)] <- 0
table(b$CommVal, useNA = "always")

################################################################################
################################################################################
#ラベルの確認
dir("05_Out_txt")

for(n in 1:max(b$CommVal)){
#n <- 1
d1 <- b[b$CommVal == n,]
#head(d1)
#head(SearchLabel)
d2 <- SearchLabel[SearchLabel$CommonEntity %in% d1$CommonEntity,]
d3 <- paste0(formatC(as.numeric(rownames(d2)), width=5, flag="0"), "_", d2$CommonEntity, "_label.csv")

for(m in 1:length(d3)){
#m <- 1
d4 <- data.frame(readr::read_csv(paste0("./05_Out_txt/", d3[m]), col_names = F))  
if(m == 1){
readr::write_csv(d4, file=paste0("./07_Out/Comm_", formatC(n, width=3, flag="0"),"_label.csv"), 
  append=F, col_names = F)
}else{
readr::write_csv(d4, file=paste0("./07_Out/Comm_", formatC(n, width=3, flag="0"),"_label.csv"), 
  append=T, col_names = F)  
}
}
}

################################################################################
################################################################################
#k <- 3
for(k in 1:122){
#k <- 1
a <- paste0("./05_Out_txt/", formatC(k, width=5, flag="0"), "_", SearchLabel$CommonEntity[k], "_label.csv")
B <- data.frame(readr::read_csv(a, col_names = F))

head(B)
B01 <- B[nchar(B$X2) == 1,]
B02 <- B[nchar(B$X2) == 2,]  
B03 <- rbind(B01, B02)

a1 <- paste0("./05_Out_txt/", formatC(k, width=5, flag="0"), "_", SearchLabel$CommonEntity[k], "_label_chr_1_2.csv")
readr::write_csv(B03, file=a1, append=F, col_names = F)  
}

################################################################################
################################################################################
################################################################################
################################################################################
#Community Labels
################################################################################
for(l in 1:12){
#l <- 1
print(l)
b[b$CommVal == l,]
ab <- b[b$CommVal == l,]
SearchLabel[SearchLabel$QID %in% ab$QID,]

b1 <- data.frame(readr::read_csv(file=paste0("./07_Out/Comm_", formatC(l, width=3, flag="0"),"_label.csv"), col_names = F))
head(b1)
b1$X5 <- paste0(b1$X1, ".", b1$X4, ".", b1$X2, ".", b1$X3)
head(b1)
b1 <- b1[order(b1$X4),]
b2 <- b1[as.numeric(rownames(unique(b1["X5"]))),-5]
head(b2)

hist(nchar(b2$X2), breaks=seq(0, 300, by=1))
table(nchar(b2$X2) == 1)
table(nchar(b2$X2) > 25)
b2[nchar(b2$X2) == 1,]
#Sys.sleep(2)
b2[nchar(b2$X2) == 2,]
b2 <- b2[order(nchar(b2$X2)),]
readr::write_csv(b2, file=paste0("./07_Out/Comm_", formatC(l, width=3, flag="0"),"_label_uni.csv"), append=F, col_names = F)

b3 <- b2[nchar(b2$X2) == 1,]
readr::write_csv(b3, file=paste0("./07_Out/Comm_", formatC(l, width=3, flag="0"),"_label_uni_chr1_Num", length(b3), ".csv"), append=F, col_names = F)

b4 <- b2[nchar(b2$X2) == 2,]
readr::write_csv(b4, file=paste0("./07_Out/Comm_", formatC(l, width=3, flag="0"),"_label_uni_chr2_Num", length(b4), ".csv"), append=F, col_names = F)

b5 <- b2[nchar(b2$X2) > 25,]
readr::write_csv(b5, file=paste0("./07_Out/Comm_", formatC(l, width=3, flag="0"),"_label_uni_chr25_Num", length(b5), ".csv"), append=F, col_names = F)
}

################################################################################
##Intermediate_concepts_v01
################################################################################
c1 <- data.frame(readr::read_csv(paste0("./06_Out_txt/Intermediate_concepts_v01.csv"), col_names = F))
head(c1)

for(n in 1:nrow(c1)){
#n <- 2
c2 <- paste0("X", c1$X1[n], c1$X2[n], ".", c1$X4[n])  
c3 <- unlist(strsplit(c1$X10[n], split=";"))

#head(b)
b[,c2] <- 0
b[c(b$Name.QID %in% c3),c2] <- 1
}

head(b)

