#データ読み込み
rm(list=ls())
#sessionInfo()

## Read data
dir("./04_Out")
xx <- dir("./04_Out", pattern = "SearchNum_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
SearchNum <- readRDS(xx[length(xx)])

xx <- dir("./04_Out", pattern = "SearchNum00_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
SearchNum00 <- readRDS(xx[length(xx)])

QID_rdfsLabel02 <- readRDS("./02_Out/R02_QID_rdfsLabel02.Rdata")
head(QID_rdfsLabel02)
############

SearchLabel <- SearchNum00[!is.na(SearchNum00$Levels),]
head(SearchLabel)

SearchLabel <- SearchLabel[order(SearchLabel$Levels),]
rownames(SearchLabel) <- 1:nrow(SearchLabel)
dim(SearchLabel)
head(SearchLabel)
tail(SearchLabel)

head(SearchLabel)
SearchLabel <- SearchLabel[SearchLabel$Levels <= 5,]
head(SearchLabel)
table(SearchLabel$Levels)
dim(SearchLabel)

#######################################################
#######################################################
##全体検索
#######################################################
if(!file.exists("./05_Out_txt")){dir.create("./05_Out_txt")}
file_path <- "./wikidata201013_v01_copy/Relation_P31_P279_df.csv"
a <- data.frame(readr::read_csv(file_path, col_names = F))
#b <- list(a)
#table(grepl("^wd:Q[1-9]", a[,1]))
#table(grepl("^wdt:P[1-9]", a[,2]))
#table(grepl("^wd:Q[1-9]", a[,3]))

#S <- a$X1
#P <- a$X2
#O <- a$X3
#rm(list="a")
#table(grepl("^wd:Q[1-9]", S))
#table(grepl("^wdt:P[1-9]", P))
#table(grepl("^wd:Q[1-9]", O))

Test01 <- function(N=1, upDepth=30){
#N <- 1; upDepth <- 30
message(paste0("CommonEntity: ", SearchLabel$CommonEntity[N]))
message(paste0("entityNamej: ", SearchLabel$entityNamej[N]))
message(paste0("Hierarchy: ", SearchLabel$Levels[N]))

#上位検索
Query <- SearchLabel$CommonEntity[N]
x <- 0

repeat{
x <- x +1
message(paste0("up: ", x))
cc <- c(a[,1] %in% Query)
b <- a[cc,]
#head(b)

if(x != 1){
b <- b[b[,2] != "wdt:P31",]  
}

if(nrow(b) > 0){
b <- data.frame(b, X4="up", X5=x, X6=SearchLabel$CommonEntity[N], X7=SearchLabel$Levels[N],
                X8=paste0(b$X1, ".", b$X2, ".", b$X3))
if(x == 1){
readr::write_csv(b, file=paste0("./05_Out_txt/", 
                           formatC(N, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[N], ".csv"), 
            append=F, col_names = F)  
}else{
readr::write_csv(b, file=paste0("./05_Out_txt/", 
                           formatC(N, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[N], ".csv"), 
            append=T, col_names = F)
}

d <- data.frame(X1=paste0(sub("wd:", "<http://www.wikidata.org/entity/", b$X1), ">"),
                X2=paste0(sub("wdt:", "<http://www.wikidata.org/prop/direct/", b$X2), ">"),
                X3=paste0(sub("wd:", "<http://www.wikidata.org/entity/", b$X3), ">"))

e <- data.frame(X=apply(d, 1, function(x) paste0(paste0(x, collapse=" "), " .")))
if(x == 1){
readr::write_csv(e,
            file=paste0("./05_Out_txt/", 
                        formatC(N, width = 5, flag = "0"), "_", 
                        SearchLabel$CommonEntity[N], ".nt"), 
            append=F, col_names = F)  
}else{
readr::write_csv(e,
            file=paste0("./05_Out_txt/", 
                        formatC(N, width = 5, flag = "0"), "_", 
                        SearchLabel$CommonEntity[N], ".nt"), 
            append=T, col_names = F)
}

if(x == upDepth){break}
}else{ break }
Query <- unique(b[,3])
}

#下位検索
Query <- SearchLabel$CommonEntity[N]
x <- 0

repeat{
x <- x +1
message(paste0("down: ", x))
cc <- c(a[,3] %in% Query)
b <- a[cc,]
#head(b)

if(x != SearchLabel$Levels[N]){
b <- b[b[,2] != "wdt:P31",]  
}

if(nrow(b) > 0){
b <- data.frame(b, X4="down", X5=x, X6=SearchLabel$CommonEntity[N], X7=SearchLabel$Levels[N],
                X8=paste0(b$X1, ".", b$X2, ".", b$X3))

readr::write_csv(b, file=paste0("./05_Out_txt/", 
                           formatC(N, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[N], ".csv"), 
  append=T, col_names = F )

d <- data.frame(X1=paste0(sub("wd:", "<http://www.wikidata.org/entity/", b$X1), ">"),
                X2=paste0(sub("wdt:", "<http://www.wikidata.org/prop/direct/", b$X2), ">"),
                X3=paste0(sub("wd:", "<http://www.wikidata.org/entity/", b$X3), ">"))

e <- data.frame(X=apply(d, 1, function(x) paste0(paste0(x, collapse=" "), " .")))

readr::write_csv(e, file=paste0("./05_Out_txt/", 
                          formatC(N, width = 5, flag = "0"), "_", 
                          SearchLabel$CommonEntity[N], ".nt"), 
  append=T, col_names = F)  
}

if(x == SearchLabel$Levels[N]){break}

Query <- unique(b[,1])
}
}

###########################################################################
###########################################################################
#逐次的実行
#system.time(for(n in c(1:nrow(SearchLabel))){
for(n in seq_len(nrow(SearchLabel))){
#n <- 1
print(paste0("No.: ", n))
try(Test01(N=n), silent=F)
#message("post-processing...")
}

###########################################################################
###########################################################################
#ファイル個別にユニーク処理
for(n in seq_len(nrow(SearchLabel))){
#n <- 1
print(paste0("No.: ", n))

#CSVファイル
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[n], ".csv"), col_names = F))
#head(A)
A1 <- A[A$X4 == "up",]; rownames(A1) <- 1:nrow(A1)
A2 <- A[A$X4 == "down",]; rownames(A2) <- 1:nrow(A2)

B1 <- A1[as.numeric(rownames(unique(A1["X8"]))),]
B2 <- A2[as.numeric(rownames(unique(A2["X8"]))),]
B3 <- rbind(B1, B2)
if(sum(c(B1$X8 %in% B2$X8)) > 0){message("Doburi")}

readr::write_csv(B3, file=paste0("./05_Out_txt/", 
                           formatC(n, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[n], "_uni.csv"), 
            append=F, col_names = F)
if(n == 1){
readr::write_csv(B3, file=paste0("./05_Out_txt/All_uni_01.csv"), 
            append=F, col_names = F)  
}else{
readr::write_csv(B3, file=paste0("./05_Out_txt/All_uni_01.csv"), 
            append=T, col_names = F)
}

#NTファイル
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/", formatC(n, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[n], ".nt"), col_names = F))
#head(A)
B <- data.frame(X1=A[as.numeric(rownames(unique(A["X1"]))),])
readr::write_csv(B, file=paste0("./05_Out_txt/", 
                           formatC(n, width=5, flag="0"), "_", 
                           SearchLabel$CommonEntity[n], "_uni.nt"), 
            append=F, col_names = F)

if(n == 1){
readr::write_csv(B, file=paste0("./05_Out_txt/All_uni_01.nt"), 
            append=F, col_names = F) 
}else{
readr::write_csv(B, file=paste0("./05_Out_txt/All_uni_01.nt"), 
            append=T, col_names = F)
}
}

###########################################################################
###########################################################################
#ファイル全体でユニーク
if(T){
#CSVファイル全体でユニーク
rm(list=ls())
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_01.csv"), col_names = F))
head(A)
B <- A[as.numeric(rownames(unique(A["X8"]))),]
readr::write_csv(B, file=paste0("./05_Out_txt/All_uni_02.csv"), 
            append=F, col_names = F)

#NTファイル全体でユニーク
rm(list=ls())
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_01.nt"), col_names = F))
head(A)
B <- data.frame(X1=A[as.numeric(rownames(unique(A["X1"]))),])
readr::write_csv(B,
            file=paste0("./05_Out_txt/All_uni_02.nt"), 
            append=F, col_names = F)
}

###########################################################################
###########################################################################
#全データの基本カウント
rm(list=ls())
A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02.csv"), col_names = F))
head(A)
dim(A)

#トリプル・カウント
length(unique(A$X8))
#7202596

#エンティティ・カウント
length(unique(c(A$X1, A$X3)))
#6306735

#プロパティ・カウント
table(A$X2)
#wdt:P279  wdt:P31 
#  917794  6284802


###########################################################################
###########################################################################
#前処理
#system("wc -l ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel.csv")
#round(86825926/4, 0) + 1
#system("split -l 21706483 ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel.csv ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s.")
#system("mv ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s.aa ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s01.csv")
#system("mv ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s.ab ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s02.csv")
#system("mv ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s.ac ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s03.csv")
#system("mv ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s.ad ./wikidata201013_v01/Label_df_rdfslabel_skosaltLabel_s04.csv")

##############################################################
##ラベル情報の抽出
##############################################################
rm(list=ls())

A <- data.frame(readr::read_csv(paste0("./05_Out_txt/All_uni_02.csv"), col_names = F))

head(A)
dim(A)

list <- paste0(sub("wd:", "<http://www.wikidata.org/entity/", unique(c(A$X1, A$X3))), ">")
str(list)

#file_path <- "./wikidata201013_v01/Label.nt"
file_path <- "/Volumes/sk/others/ToDo_210311/wikidata201013_v01/Label.nt"
N=100000

con <- file(description = file_path, open = "r")
n <- 0

#実行
repeat {
    n <- n + 1

    message(paste0("No.: ", n*N))
    a <- readLines(con, n = N)
    if ( length(a) == 0 ) { break }
    
    #str(a)
    a1 <- apply(data.frame(X1=a), 1, function(x) strsplit(x, " ")[[1]][1])
    a2 <- a[c(a1 %in% list)]
    
    a3 <- a2[grepl("<http://www.w3.org/2000/01/rdf-schema#label>", a2)]
    a4 <- c(a3, a2[grepl("<http://www.w3.org/2004/02/skos/core#altLabel>", a2)])
    #a5 <- c(a4, a2[grepl("<http://schema.org/description>", a2)])
    
    if(!identical(a4, character(0))){
    if( n == 1 ){
      readr::write_delim(a4,
            file=paste0("./05_Out_txt/All_uni_02_label.nt"), 
            delim="", append=F, col_names = F)
    }else{
      readr::write_delim(a4,
            file=paste0("./05_Out_txt/All_uni_02_label.nt"), 
            delim="", append=T, col_names = F)
    }}
    
    #head(a4)
    if(!identical(a4, character(0))){
    b1 <- data.frame(matrix(unlist(apply(data.frame(X1=a4), 1, 
      function(x) strsplit(x, "<http://www.w3.org/2000/01/rdf-schema#label>|<http://www.w3.org/2004/02/skos/core#altLabel>")[[1]])), 
      ncol=2, byrow=T))

    #head(b1)
    b1$X3 <- NA
    b1$X4 <- NA
    
    b1$X4[grepl("<http://www.w3.org/2000/01/rdf-schema#label>", a4)] <- "rdfs:label"
    b1$X4[grepl("<http://www.w3.org/2004/02/skos/core#altLabel>", a4)] <- "skos:altLabel"
    
    b1$X1 <- sub("^<http://www.wikidata.org/entity/", "wd:", b1$X1)
    b1$X1 <- sub("> $", "", b1$X1)
    b1$X2 <- sub(" .$", "", b1$X2)
    
    b1$X3[grepl("@ja$", b1$X2)] <- "ja"
    b1$X3[grepl("@en$", b1$X2)] <- "en"
    b1$X2 <- sub("@ja$", "", b1$X2)
    b1$X2 <- sub("@en$", "", b1$X2)
    b1$X2 <- sub('"$', '', b1$X2)
    b1$X2 <- sub('^ "', '', b1$X2)
    #head(b1); table(b1$X4)
    if( n == 1 ){
     readr::write_csv(b1,
        file=paste0("./05_Out_txt/All_uni_02_label.csv"), 
        append=F, col_names = F)
    }else{
     readr::write_csv(b1,
        file=paste0("./05_Out_txt/All_uni_02_label.csv"), 
        append=T, col_names = F)
    }
  }
}

close(con)


###########################################################################
#各起点範囲のカウント
###########################################################################
rm(list=ls())

#Folder <- "05_Out_txt"
#A <- data.frame(readr::read_csv(paste0("./", Folder, "/All_uni_02.csv"), col_names = F))
#head(A)
#dim(A)

################################################################################
Folder <- "05_Out_txt"
xx <- dir("./04_Out", pattern = "SearchNum00_[1-9][0-9][0-9][0-9][0-9][0-9].Rdata", full.names = T)
SearchNum00 <- readRDS(xx[length(xx)])
dim(SearchNum00)
################################################################################
SearchLabel <- SearchNum00[!is.na(SearchNum00$Levels),]
SearchLabel <- SearchLabel[order(SearchLabel$Levels),]
rownames(SearchLabel) <- 1:nrow(SearchLabel)
SearchLabel$entityNamej[is.na(SearchLabel$entityNamej)] <- SearchLabel$entityNamee[is.na(SearchLabel$entityNamej)]

head(SearchLabel)
dim(SearchLabel)

#ALL
#SearchLabel <- SearchLabel[SearchLabel$Levels > 0,]
#５段以下
SearchLabel <- SearchLabel[SearchLabel$Levels <= 5,]

head(SearchLabel)
tail(SearchLabel)
dim(SearchLabel)
table(SearchLabel$Levels)

a <- data.frame(matrix(NA, ncol=12, nrow=nrow(SearchLabel)))

#実行
for(n in 1:nrow(SearchLabel)){
#n <- 1
print(n)

B <- data.frame(readr::read_csv(paste0("./", Folder, "/", 
                formatC(n, width=5, flag="0"), "_", 
                SearchLabel$CommonEntity[n], ".csv"), col_names = F))
#head(B)
a[n,1] <- n
a[n,2] <- SearchLabel$CommonEntity[n]
a[n,3] <- SearchLabel$entityNamej[n]

#全体
a[n,4] <- length(unique(B$X8))
a[n,5] <- length(unique(c(B$X1, B$X3)))

#up
cc <- B[B$X4 == "up",]
cc1 <- unique(c(cc$X1, cc$X3))
cc2 <- cc1[!c(cc1 %in% SearchLabel$CommonEntity[n])]
a[n,6] <- length(cc2)

#down
cc <- B[B$X4 == "down",]
cc1 <- unique(c(cc$X1, cc$X3))
cc2 <- cc1[!c(cc1 %in% SearchLabel$CommonEntity[n])]
a[n,7] <- length(cc2)

#段数
a[n,8] <- max(B[B$X4 == "up",]$X5)
a[n,9] <- max(B[B$X4 == "down",]$X5)

C <- B[B$X4 == "down",]
#head(C)
b <- SearchLabel[SearchLabel$QID %in% unique(c(C$X1, C$X3)),]
b.c <- b[!c(b$QID %in% SearchLabel$CommonEntity[n]),]

a[n,10] <- paste0(SearchLabel$entityNamej[n], "(", SearchLabel$CommonEntity[n], ")")
a[n,11] <- nrow(b.c)

if( nrow(b.c) == 0 ){
a[n,12] <- "" 
}else{
a[n,12] <- paste0(b.c$entityNamej, "(", b.c$QID, ")", collapse="; ")  
}

#head(a)
colnames(a) <- c("No", 
  "CommonEntity", 
  "entityNamej", 
  "TripleNum", 
  "AllEntityNum",
  "up.EntityNum",
  "down.EntityNum",
  "up.DanNum",
  "down.DanNum",
  "entityNamej",
  "down.CommonEntityNum",
  "CommonEntitylist")

readr::write_excel_csv(a,
                       file=paste0("./", Folder, "/list.csv"), 
                       append=F, col_names=F)
}

#####################################################################
#####################################################################

