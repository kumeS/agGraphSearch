#Unicodeエスケープの変換
#system("which nkf")
#system("brew install nkf")
#cd /Users/sas/Desktop/ToDO_210205/01_2_Onto_test_PolyInfo_Ver1.1/05_Out_txt
#cat ./All_uni_02_label.nt | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./All_uni_02_label_nkf.nt; head ./All_uni_02_label_nkf.nt
#cat ./All_uni_02_label.csv | sed 's/\\\u\(....\)/\&#x\1;/g' | nkf --numchar-input -w > ./All_uni_02_label_nkf.csv; head ./All_uni_02_label_nkf.csv


#ラベルの読み込み
rm(list=ls())
library(magrittr)
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02_label_nkf.csv"), col_names = F))
head(A)
dim(A)
table(is.na(A))
A <- data.frame(na.omit(A))
str(A)

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

#順番
head(A)

A01 <- A[order(nchar(A$X2), decreasing = F),]
head(A01)
rownames(A01) <- 1:nrow(A01)

A00 <- c()
abc <- nchar(A01$X2)
for(n in 1:max(nchar(A$X2))){
#n <- 1
print(n)
a00 <- A01[abc == n,]
if(nrow(a00) > 0){
rownames(a00) <- 1:nrow(a00)
#head(a00)
a01 <- data.frame(X=1:nrow(a00), Y=as.numeric(sub("wd:Q", "", a00$X1)))
#head(a01)
a01 <- a01[order(a01$Y, decreasing = F),]
a02 <- a00[as.numeric(a01$X),]
#head(a02)
A00 <- A00 %>% rbind(a02) 
}
}

A01 <- A00
A02 <- A01[A01$X4 == "rdfs:label",]
A03 <- A01[A01$X4 == "skos:altLabel",]
##
A04 <- A02[A02$X3 == "ja",]
A05 <- A02[A02$X3 == "en",]
A06 <- A03[A03$X3 == "ja",]
A07 <- A03[A03$X3 == "en",]

B <- A04 %>% rbind(A05) %>% rbind(A06) %>% rbind(A07)
rownames(B) <- 1:nrow(B)
head(B)

if(!file.exists("./08_Out")){dir.create("./08_Out")}
#saveRDS(B, "./08_Out/B.Rdata")
rm(list=ls())
B <- readRDS("./08_Out/B.Rdata")
#全体
nrow(B)
#重複除く
length(unique(B$X2))

####################################################################




#文字列
range(nchar(A$X2))
summary(nchar(A$X2))
hist(nchar(A$X2), breaks=50)

#1文字
A01 <- A[nchar(A$X2) == 1,]
head(A01)



#確認
a <- grepl("[0-9]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[a-z]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[A-Z]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[[:alpha:]]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[[:space:]]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[あ-ん]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[ア-ン]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[:digit:]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[:alnum:]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- grepl("[:punct:]", A01$X2); A01[a,]
A01 <- A01[!a,]
a <- stringr::str_detect(A01$X2, "\\p{Hiragana}"); A01[a,]
A01 <- A01[!a,]
a <- stringr::str_detect(A01$X2, "\\p{Katakana}"); A01[a,]
A01 <- A01[!a,]

a <- stringr::str_detect(A01$X2, "\\p{Han}"); A01[a,]

A01[a,]; A01[!a,]

#漢字:"\\p{Han}"
#ひらがな:"\\p{Hiragana}",カタカナ:"\\p{Katakana}"
#[あ-ん]
#[ア-ン]
#[:alnum:] アルファベットと数値、[:alpha:] + [:digit:]
#[:alpha:] 大小文字アルファベット、 [:lower:] + [:upper:]
#[:lower:] 小文字アルファベ
#[:digit:] 数値
#[:blank:] 空白文字、スペースとタブ
#[:cntrl:] 制御文字(000-037, 177('DEL')
#[:graph:] グラフィカル文字 ([:alnum:] と [:punct:])|
#[:print:] 印字可能な文字、[:alnum:] + [:punct:] + space
#[:punct:] パンクチュエーション文字 ! " # $ % & ' ( ) * + , - . /
#[:space:] 空白文字、タブ、改行、水平タブ、給紙、キャリッジリターン、空白
#[:xdigit:] 16進数 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f

#https://qiita.com/uri/items/1aedc5030acc5d3691c2
#https://www.karada-good.net/analyticsr/r-522
#http://www.okadajp.org/RWiki/?R+%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE

#25文字以上の確認

#rdfs:labelは前半、別名は後半にする



#レベルが低い起点のIDから並び替え




