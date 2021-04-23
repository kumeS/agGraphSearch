#準備
#P31とP279のみのNTファイル
#system("wc -l ./wikidata201013_v01/Relation_P31_P279.nt")
#94352764
###############################################################################
options(digits=20, scipen=1)
###############################################################################
if(F){
#rm(list=ls())
file_path <- "./wikidata201013_v01/Relation_P31_P279.nt"
con_file <- file(description = file_path, open = "r")
con_file

x <- 0
N <- 100000

while( TRUE ){
x <- x + 1
print(paste0("No: ", x, " Line: ", x*N ))
try(a <- readLines(con_file, n = N), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

a1 <- unlist(strsplit(sub(" ", "AiBiCiDiEiFiG", sub(" ", "AiBiCiDiEiFiG", a)), split="AiBiCiDiEiFiG"))
if( (length(a1) %% 3) == 0 ){
  b <- matrix(a1, ncol=3, byrow=T)
}else{
  b <- matrix(a1[1:(length(a1) - (length(a1) %% 3))], ncol=3, byrow=T)
}

#head(b)
#dim(b)
#b <- b[!grepl("@ja ", b[,3]),]
#b <- b[!grepl("@en ", b[,3]),]
b[,3] <- sub(" .$", "", b[,3])

#table(b[,2])
#table(grepl("^<http://www.wikidata.org/entity/Q", b[,1]))
c <- grepl("^<http://www.wikidata.org/entity/Q", b[,1])
b[c,1] <- sub("^<http://www.wikidata.org/entity/", "wd:", b[c,1])
b[c,1] <- sub(">$", "", b[c,1])
b <- b[grepl("^wd:Q", b[,1]),]

#table(grepl("^<http://www.wikidata.org/prop/direct/P", b[,2]))
c <- grepl("^<http://www.wikidata.org/prop/direct/P", b[,2])
b[c,2] <- sub("^<http://www.wikidata.org/prop/direct/", "wdt:", b[c,2])
b[c,2] <- sub(">$", "", b[c,2])
b <- b[grepl("^wdt:P", b[,2]),]

#table(grepl("^<http://www.wikidata.org/entity/Q", b[,3]))
#b[!c,3]
c <- grepl("^<http://www.wikidata.org/entity/Q", b[,3])
b[c,3] <- sub("^<http://www.wikidata.org/entity/", "wd:", b[c,3])
b[c,3] <- sub(">$", "", b[c,3])
b <- b[grepl("^wd:Q", b[,3]),]

#head(b)
#dim(b)
write.table(b, file=paste0(sub(".nt", "", file_path), "_df.csv"),
            sep=",", append=T, row.names=F, col.names = F, quote = T)
}

###############################################################################
###############################################################################
#ラベルのみのNTファイル
#system("wc -l ./wikidata201013_v01/Label.nt")
#376083198
###############################################################################
options(digits=20, scipen=1)
###############################################################################
file_path1 <- "./wikidata201013_v01/Label.nt"
con_file <- file(description = file_path1, open = "r")
con_file

x <- 0
N <- 100000

while( TRUE ){
x <- x + 1
print(paste0("No: ", x, " Line: ", x*N ))
try(a <- readLines(con_file, n = N), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

a1 <- unlist(strsplit(sub(" ", "AiBiCiDiEiFiG", sub(" ", "AiBiCiDiEiFiG", a)), split="AiBiCiDiEiFiG"))
if( (length(a1) %% 3) == 0 ){
  b <- matrix(a1, ncol=3, byrow=T)
}else{
  b <- matrix(a1[1:(length(a1) - (length(a1) %% 3))], ncol=3, byrow=T)
}

#head(b, n=100)
#b <- b[!grepl("@ja ", b[,3]),]
#b <- b[!grepl("@en ", b[,3]),]
b[,3] <- sub(" .$", "", b[,3])

#head(b, n=10)
b <- b[b[,2] != "<http://schema.org/name>",]
b <- b[b[,2] != "<http://schema.org/description>",]

b[,1] <- sub("^<http://www.wikidata.org/entity/", "", b[,1])
b[,1] <- sub(">$", "", b[,1])

b[,2] <- sub("<http://www.w3.org/2000/01/rdf-schema#label>", "rdfs:label", b[,2])
b[,2] <- sub("<http://www.w3.org/2004/02/skos/core#prefLabel>", "skos:prefLabel", b[,2])
b[,2] <- sub("<http://www.w3.org/2004/02/skos/core#altLabel>", "skos:altLabel", b[,2])

#head(b, n = 10)
#dim(b)
write.table(b, file=paste0(sub(".nt", "", file_path1), "_df.csv"),
            sep=",", append=T, row.names=F, col.names = F, quote = T)
}

###############################################################################
###############################################################################
#その他のNTファイル
#system("wc -l ./wikidata201013_v01/Relation_others.nt")
#
###############################################################################
options(digits=20, scipen=1)
###############################################################################
file_path2 <- "./wikidata201013_v01/Relation_others.nt"
con_file <- file(description = file_path2, open = "r")
con_file

x <- 0
N <- 100000

while( TRUE ){
x <- x + 1
print(paste0("No: ", x, " Line: ", x*N ))
try(a <- readLines(con_file, n = N), silent=T)
if ( length(a) == 0 ) { close(con_file); break }

a1 <- unlist(strsplit(sub(" ", "AiBiCiDiEiFiG", sub(" ", "AiBiCiDiEiFiG", a)), split="AiBiCiDiEiFiG"))
if( (length(a1) %% 3) == 0 ){
  b <- matrix(a1, ncol=3, byrow=T)
}else{
  b <- matrix(a1[1:(length(a1) - (length(a1) %% 3))], ncol=3, byrow=T)
}

#head(b, n=200)
#b <- b[!grepl("@ja ", b[,3]),]
#b <- b[!grepl("@en ", b[,3]),]
b[,3] <- sub(" .$", "", b[,3])

#table(b[,2])
#table(grepl("^<http://www.wikidata.org/entity/Q", b[,1]))
c <- grepl("^<http://www.wikidata.org/entity/Q[1-9]", b[,1])
#table(c)
b[c,1] <- sub("^<http://www.wikidata.org/entity/", "wd:", b[c,1])
b[c,1] <- sub(">$", "", b[c,1])
b <- b[grepl("^wd:Q[1-9]", b[,1]),]
#head(b, n=200)

#table(grepl("^<http://www.wikidata.org/prop/direct/P", b[,2]))
c <- grepl("^<http://www.wikidata.org/prop/direct/P[1-9]", b[,2])
b[c,2] <- sub("^<http://www.wikidata.org/prop/direct/", "wdt:", b[c,2])
b[c,2] <- sub(">$", "", b[c,2])
#b <- b[grepl("^wdt:P[1-9]", b[,2]),]
#head(b, n=200)

#table(grepl("^<http://www.wikidata.org/entity/Q", b[,3]))
c <- grepl("^<http://www.wikidata.org/entity/Q[1-9]", b[,3])
b[c,3] <- sub("^<http://www.wikidata.org/entity/", "wd:", b[c,3])
b[c,3] <- sub(">$", "", b[c,3])

#head(b, n=200)
#dim(b)
write.table(b, file=paste0(sub(".nt", "", file_path2), "_df.csv"),
            sep=",", append=T, row.names=F, col.names = F, quote = T)
}
}
