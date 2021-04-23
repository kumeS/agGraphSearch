rm(list=ls())

###前処理
words <- read.table("./00_Input/PoLyInfo_metadata_ALL_v02.csv",
                       sep=",", header=T, fill = TRUE, na.strings = "NA", 
                       check.names = F, stringsAsFactors=F)[,-3]
wordsj <- read.table("./00_Input/PoLyInfo_metadata_Jpn_v03.csv",
                       sep=",", header=T, fill = TRUE, na.strings = "NA", 
                       check.names = F, stringsAsFactors=F)

#head(words)
#head(wordsj)
#head(Wikidata)
#Wikidata1 <- Wikidata

#head(SudachiABC)
#dim(SudachiABC)
#head(words)
#dim(words)
#head(wordsj)
#dim(wordsj)
#length(as.character(unlist(wordsj)[unlist(wordsj) != ""]))

#head(Wikidata1)
#dim(Wikidata1)

#確認1
abc <- data.frame(abc=as.character(unlist(wordsj)[unlist(wordsj) != ""]))
#abc

if(F){
abc[as.numeric(rownames(unique(abc["abc"]))),]
abc[-as.numeric(rownames(unique(abc["abc"]))),]
}

#確認2
def <- data.frame(def=as.character(unlist(words)[unlist(words) != ""]))
def <- def[-c(1:421),]
def <- data.frame(def=def)

if(F){
def[as.numeric(rownames(unique(def["def"]))),]
def[-as.numeric(rownames(unique(def["def"]))),]
}

#
words.c <- unique(as.character(unlist(words)[unlist(words) != ""]))
wordsj.c <- unique(as.character(unlist(wordsj)[unlist(wordsj) != ""]))
words.cc <- unique(c(wordsj.c, words.c))

if(F){
words.c
head(words.c)
length(words.c)

#英語 length(419:(length(words.c)))
head(wordsj.c)
length(wordsj.c)

head(words.cc)
length(words.cc)
}

#日本語: 
length(words.cc) - length(516:(length(words.cc)))
#英語: length(516:(length(words.cc)))
#head(Wikidata1.c)
#length(Wikidata1.c)

###################################################################
if(F){
#日本語ラベルのみ
length(words.cc)
words.cc
}

words.lab <- words.cc[1:515]
words.lab
head(words.lab)
length(words.lab)

#英語
#words.lab <- words.c[333:750]
###################################################################

#SPARQL検索ラベルの作成
readr::write_excel_csv(data.frame(words.lab), file="./00_Input/words.lab.csv", col_names = T)

