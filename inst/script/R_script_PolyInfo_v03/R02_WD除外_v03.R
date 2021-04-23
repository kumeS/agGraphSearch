rm(list=ls())
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")

#並列
plan(multisession(workers = 4))
plan()

QID_rdfsLabel <- readRDS("./01_Out/R01_QID_rdfsLabel.Rdata")
dim(QID_rdfsLabel)
head(QID_rdfsLabel)

#######################################################
# 関数定義
#######################################################
Tem00 <- function(WDname0="wd:Q12159869", Object0="wd:Q101352", Property0="?p", Count0="?p"){
Val00 <- agCount_QID_Property_Object_v02(WDname=WDname0, Property=Property0, Object=Object0, Count=Count0)
return(Val00)
}

Tem01 <- function(WDname0="wd:Q12159869", Property0="?p", Object0="?o", Count0="?o"){
Val00 <- agCount_QID_Property_Object_v02(WDname=WDname0, Property=Property0, Object=Object0, Count=Count0)
return(Val00)
}

Tem02 <- function(WDname00="wd:Q12159869", Property00="wdt:P31"){
agWD_QID_property_Object_v01(WDname=WDname00, Property=Property00)  
}

#######################################################
#QIDで除外
#######################################################
QID_rdfsLabel00 <- QID_rdfsLabel
head(QID_rdfsLabel00)

#QID・エンティティのカウント
#Family name(wd:Q101352)
#Taxon(wd:Q16521)
#映画(wd:Q11424)
#国(wd:Q6256)
#男性の名前(wd:Q12308941)
#女性の名前(wd:Q11879590)
#ヒト(wd:Q5)
#町丁(wd:Q5327369)
#年(wd:Q577)
#ウィキメディアの曖昧さ回避ページ(Q4167410)
#絵画作品(Q3305213)
#ウィキメディアの一覧記事(Q13406463)
#エンティティ(Q35120)
#記号 (Q3695082)
#知的財産権 (Q131257)
#シンボル (Q80071)
#企業 (Q4830453)
#アルバム (Q482994)
#音楽用語 (Q20202269)
#生物学的プロセス (Q2996394)

ExcluQ <- c("Q101352", "Q16521", "Q11424", "Q6256", 
            "Q12308941", "Q11879590", "Q5", "Q5327369", 
            "Q577", "Q4167410", "Q3305213", "Q13406463", 
            "Q35120", "Q3695082", "Q131257", "Q80071", 
            "Q4830453", "Q482994", "Q20202269", "Q2996394")

NumQ <- length(ExcluQ)
if(!dir.exists("02_Out")){dir.create("02_Out")}

for(n in seq_len(NumQ)){
#n <- 2
print(ExcluQ[n])
DatA <- QID_rdfsLabel00$QID; DatB <- rep(paste0("wd:", ExcluQ[n]), length(DatA))
a <- furrr::future_map2(unlist(DatA), unlist(DatB), Tem00, .progress = TRUE)
table(unlist(a))
eval(parse(text=paste0("QID_rdfsLabel00$", ExcluQ[n], " <- c(unlist(a) > 0)")))
head(QID_rdfsLabel00)
message("")
readr::write_excel_csv(QID_rdfsLabel00, 
                       paste0("./02_Out/R02_", formatC(n, width = 3, flag = "0"), "_", ExcluQ[n], ".csv"), col_names = T)
print(eval(parse(text=paste0("QID_rdfsLabel00[QID_rdfsLabel00$", ExcluQ[n], " == TRUE,c(1,2,ncol(QID_rdfsLabel00))]"))))
QID_rdfsLabel00 <- QID_rdfsLabel00[unlist(a) == 0,]
print(dim(QID_rdfsLabel00))
}

head(QID_rdfsLabel00)
dim(QID_rdfsLabel00)

##プロパティのカウント
##位置する行政区画(wdt:P131)
##性別(wdt:P21)
#版(wdt:P747)
#家畜の品種 (wdt:P4743)
#出版日 (wdt:P577)

ExcluP <- c("P131","P21","P747","P4743","P577")
NumP <- length(ExcluP)

for(n in seq_len(NumP)){
#n <- 1
print(ExcluP[n])
DatA <- QID_rdfsLabel00$QID; DatB <- rep(paste0("wdt:", ExcluP[n]), length(DatA))
a <- furrr::future_map2(unlist(DatA), unlist(DatB), Tem01, .progress = TRUE)
table(unlist(a))
eval(parse(text=paste0("QID_rdfsLabel00$", ExcluP[n], " <- c(unlist(a) > 0)")))
head(QID_rdfsLabel00)
readr::write_excel_csv(QID_rdfsLabel00, 
                       paste0("./02_Out/R02_", formatC(n + NumQ, width = 3, flag = "0"), 
                              "_", ExcluP[n], ".csv"), col_names = T)
print(eval(parse(text=paste0("QID_rdfsLabel00[QID_rdfsLabel00$", ExcluP[n], " == TRUE,c(1,2,ncol(QID_rdfsLabel00))]"))))
QID_rdfsLabel00 <- QID_rdfsLabel00[unlist(a) == 0,]
print(dim(QID_rdfsLabel00))
}

head(QID_rdfsLabel00)
dim(QID_rdfsLabel00)

#################################################################################
##除去の確認
#################################################################################
head(QID_rdfsLabel00)
b <- apply(QID_rdfsLabel00[,c(4:ncol(QID_rdfsLabel00))], 1, any)
table(b)

#新規の変数を作成
QID_rdfsLabel01 <- QID_rdfsLabel00
head(QID_rdfsLabel01)
QID_rdfsLabel01
dim(QID_rdfsLabel01)

#行名のリネイム、4列目以降を除外
rownames(QID_rdfsLabel01) <- 1:nrow(QID_rdfsLabel01)
QID_rdfsLabel02 <- QID_rdfsLabel01[,-c(4:ncol(QID_rdfsLabel01))]

dim(QID_rdfsLabel02)
head(QID_rdfsLabel02)

#QID数
length(unique(QID_rdfsLabel02$QID))
#ラベル数
length(unique(QID_rdfsLabel02$entityNamej))
length(unique(QID_rdfsLabel02$entityNamee))

saveRDS(QID_rdfsLabel02, file = paste0("./02_Out/R02_QID_rdfsLabel02.Rdata"), compress = TRUE)
readr::write_excel_csv(data.frame(QID_rdfsLabel02), paste0("./02_Out/R02_QID_rdfsLabel02.csv"), col_names = T)

#################################################################################
#################################################################################
rm(list=ls())
QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")
dim(QID_rdfsLabel02)
head(QID_rdfsLabel02)

#QID重複からの確認
head(QID_rdfsLabel03 <- QID_rdfsLabel02[,1:2])
head(QID_rdfsLabel03)

aa <- names(table(QID_rdfsLabel03$entityNamej))[table(QID_rdfsLabel03$entityNamej)>1]
QID_rdfsLabel03.in <- QID_rdfsLabel03[c(QID_rdfsLabel03$entityNamej %in% aa),]
head(QID_rdfsLabel03.in)
QID_rdfsLabel03.in <- QID_rdfsLabel03.in[order(QID_rdfsLabel03.in$entityNamej),]
readr::write_excel_csv(QID_rdfsLabel03.in, paste0("./02_Out/R02_dob.csv"), col_names = T)

#プロパティからの確認
#並列
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")
plan(multisession(workers = 4))
plan()

DatA <- QID_rdfsLabel02$QID
a <- furrr::future_map(unlist(DatA), agWD_QID_Prop_Obj_v01, .progress = TRUE)

head(a)
b <- unlist(map(a, function(x){paste(x[,1], x[,2], sep=".")}))
c <- table(unlist(b))
c <- c[order(c, decreasing = T)]
Ratio <- round(c/length(QID_rdfsLabel02$QID), 4)

readr::write_excel_csv(data.frame(Name=names(c), Count=as.numeric(c), Ratio=as.numeric(Ratio)),
                       paste0("./02_Out/R02_property.csv"), col_names = T)

#################################################################################
#再確認
#################################################################################
rm(list=ls())
source("../00_R_Script_gist/SPARQL_AllegroGraph_v06.R")
source("../00_R_Script_gist/SPARQL_wikiWD_v02.R")

QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")
dim(QID_rdfsLabel02)
head(QID_rdfsLabel02)

#並列
plan(multisession(workers = 4))
plan()

#QIDのカウント
QID_rdfsLabel02.f <- future_map(unlist(QID_rdfsLabel02$QID), agCount_QID_Num, .progress = TRUE)
c <- c(); for(n in 1:length(QID_rdfsLabel02.f)){c <- c %>% rbind(QID_rdfsLabel02.f[[n]])}

head(c)
QID_rdfsLabel02.c <- data.frame(QID_rdfsLabel02, c)
head(QID_rdfsLabel02.c)
table(QID_rdfsLabel02.c$Count_Of_P279_P31_up)

##QIDのインスタンス
QID_rdfsLabel02.p1 <- future_map(unlist(QID_rdfsLabel02$QID), agWD_QID_property_Object_v01, .progress = TRUE)
d1 <- c(); for(n in 1:length(QID_rdfsLabel02.p1)){d1 <- d1 %>% rbind(QID_rdfsLabel02.p1[[n]])}

#QID_rdfsLabel02.p2 <- future_map(unlist(QID_rdfsLabel02$QID), wikiWD_QID_PropertyP31_Object_v01, .progress = TRUE)
#d2 <- c(); for(n in 1:length(QID_rdfsLabel02.p2)){d2 <- d2 %>% rbind(QID_rdfsLabel02.p2[[n]])}

head(d1)
#head(d2)
dim(d1)
table(d1$oLabelj)
#dim(d2)

