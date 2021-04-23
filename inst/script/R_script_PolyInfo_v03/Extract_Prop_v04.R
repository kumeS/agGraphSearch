###############################################################################
## 関係性の集計
## wdリストの読み込み
###############################################################################
rm(list=ls())
Direc <- "PolyOnt_ver1_R06_Results_SPARQL_v4_201008"
A <- dir(Direc, pattern=".Rdata")
C <- c()
library(magrittr)
for(m in seq_len(length(A))){
print(m)
print(A[m])
B <- readRDS(paste(Direc, "/", A[m], sep=""))
colnames(B) <- c("subject", "subjectLabel", 
                 "parentClass", "parentClassLabel",
                 "group" , "Depth" , "Unique"  , "UpDown" )
C <- C %>% rbind(B)
}
###############################################################################
###############################################################################
head(C)
QID <- unique(c(C$subject, C$parentClass))
head(QID)
length(QID)

QID00 <- QID[grepl("^wd:", QID)]
head(QID00)
QID01 <- sub("wd:", "", QID00)
head(QID01)
QID02 <- paste0(QID01, ">")
head(QID02)
###############################################################################
###############################################################################
###############################################################################
args = "Relation_others_s01.nt"
file_path <- paste0("./wikidata201013_v01/", args )

##主語か目的語かで、エンティティ・マッチ判定
a <- readr::read_tsv(file_path, col_names = F)

#str(a)
head(a)

for(n in seq_len(length(a$X1))){
#n <- 260
#print(n)
#a$X1[n]
b <- strsplit(sub(" ", "AiBiCiDiEiFiG", sub(" ", "AiBiCiDiEiFiG", a$X1[n])), split="AiBiCiDiEiFiG")[[1]]

#b[1]
b1 <- strsplit(b[1], split="/")[[1]]
c <- b1[length(b1)] %in% QID02

#b[3]
b2 <- sub(" .$", "", b[3])
b3 <- strsplit(b2, split="/")[[1]]
d <- b3[length(b3)] %in% QID02

if(any(c, d)){
write.table(a$X1[n], file="match_v03.nt", append=T, row.names=F, col.names = F, quote =F)  
}
}

rm(list=ls())





