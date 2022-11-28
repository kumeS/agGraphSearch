library(agGraphSearch)
library(furrr)
library(readr)
library(magrittr)
library(igraph)

## Generate directories
if(!dir.exists("04_Wiki_PolyInfo_00")){dir.create("04_Wiki_PolyInfo_00")}
if(!dir.exists("04_Wiki_PolyInfo_01")){dir.create("04_Wiki_PolyInfo_01")}
if(!dir.exists("04_Wiki_PolyInfo_02")){dir.create("04_Wiki_PolyInfo_02")}
if(!dir.exists("04_Wiki_PolyInfo_03")){dir.create("04_Wiki_PolyInfo_03")}
if(!dir.exists("04_Wiki_PolyInfo_04")){dir.create("04_Wiki_PolyInfo_04")}
if(!dir.exists("04_Wiki_PolyInfo_05")){dir.create("04_Wiki_PolyInfo_05")}
if(!dir.exists("04_Wiki_PolyInfo_06")){dir.create("04_Wiki_PolyInfo_06")}
if(!dir.exists("04_Wiki_PolyInfo_07")){dir.create("04_Wiki_PolyInfo_07")}
if(!dir.exists("04_Wiki_PolyInfo_XX")){dir.create("04_Wiki_PolyInfo_XX")}


#Temp Function for excluding the adjacent entity
Tem00 <- function(Entity_ID0, Object0, Property0="?p", Count0="?p"){
Val00 <- wikiCount_QID_Property_Object(Entity_ID=Entity_ID0, Property=Property0, Object=Object0, Count=Count0)
return(Val00)
}

SearchEntity <- function(){

if(!exists("NumQ")){return(message("Warning: NumQ does not exist"))}
if(!exists("ExcluQ")){return(message("Warning: ExcluQ does not exist"))}

for(n in seq_len(NumQ)){
#n <- 1
message(n, " / ", length(NumQ), ": ", ExcluQ[n])
if(grepl("^wd:", ExcluQ[n])){
 DatA <- QID.d$QID; DatB <- rep(ExcluQ[n], length(DatA))
}else{
 DatA <- QID.d$QID; DatB <- rep(paste0("wd:", ExcluQ[n]), length(DatA))
}
a <- furrr::future_map2(unlist(DatA), unlist(DatB), Tem00, .progress = TRUE)
eval(parse(text=paste0("QID.d$", ExcluQ[n], " <- c(unlist(a) > 0)")))

readr::write_excel_csv(QID.d,
                       paste0("./04_Wiki_PolyInfo_01/ExcluQ_", formatC(n, width = 3, flag = "0"), "_", ExcluQ[n], ".csv"),
                       col_names = T)
#show hits
print(eval(parse(text=paste0("QID.d[QID.d$", ExcluQ[n], " == TRUE,c(1, ncol(QID.d))]"))))

}

return(QID.d)

}


#Temp Function for excluding the particular property
Tem01 <- function(Entity_ID0, Property0, Object0="?o", Count0="?o"){
Val01 <- wikiCount_QID_Property_Object(Entity_ID=Entity_ID0, Property=Property0, Object=Object0, Count=Count0)
return(Val01)
}

SearchProperty <- function(){

if(!exists("NumP")){return(message("Warning: NumP does not exist"))}
if(!exists("ExcluP")){return(message("Warning: ExcluP does not exist"))}

for(n in seq_len(NumP)){
#n <- 1
message(n, " / ", length(NumP), ": ", ExcluP[n])
if(grepl("^wdt:", ExcluP[n])){
  DatA <- QID.d$QID; DatB <- rep(ExcluP[n], length(DatA))
}else{
  DatA <- QID.d$QID; DatB <- rep(paste0("wdt:", ExcluP[n]), length(DatA))
}

a <- furrr::future_map2(unlist(DatA), unlist(DatB), Tem01, .progress = TRUE)
eval(parse(text=paste0("QID.d$", ExcluP[n], " <- c(unlist(a) > 0)")))

readr::write_excel_csv(QID.d,
                       paste0("./04_Wiki_PolyInfo_01/ExcluP_", formatC(n, width = 3, flag = "0"), "_", ExcluP[n], ".csv"),
                       col_names = T)
#show hits
print(eval(parse(text=paste0("QID.d[QID.d$", ExcluP[n], " == TRUE,c(1, ncol(QID.d))]"))))
}

return(QID.d)

}

#Search for Upper Concepts
upperSearch <- function(Depth = 30){

if(!exists("QID.list")){return(message("Warning: QID.list does not exist"))}
if(!exists("ClassHi")){return(message("Warning: ClassHi does not exist"))}

QID.dat <- NULL

#Run
for(n in 1:length(QID.list)){
#n <- 1
message("No.", n, ": ", QID.list[n])
Dat <- c(); x <- 0; Query <- QID.list[n]

repeat{
x <- x + 1
message("x: ", x)

#Both wdt:P279 and wdt:P31 when x = 1
b1 <- ClassHi[ClassHi$Subject %in% Query,]

#Only wdt:P279 when x >= 2
if(x >= 2){
  b1 <- b1[b1$Property == "wdt:P279",]
}

if(nrow(b1) == 0){
  rownames(Dat) <- 1:nrow(Dat)
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Property, ".", Dat$Object)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),-c(ncol(Dat))]
  rownames(Dat) <- 1:nrow(Dat)
  #head(Dat)
  QID.dat[[n]] <- Dat
  names(QID.dat)[n] <- QID.list[n]
  break
  }
Dat <- rbind(Dat, b1)
Query <- b1$Object
if(x == as.numeric(Depth)){
  rownames(Dat) <- 1:nrow(Dat)
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Property, ".", Dat$Object)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),-c(ncol(Dat))]
  rownames(Dat) <- 1:nrow(Dat)
  #head(Dat)
  QID.dat[[n]] <- Dat
  names(QID.dat)[n] <- QID.list[n]
  break
}}}

return(QID.dat)

}

## Visualize Individual network diagrams.

VisualizeGraph <- function(){

if(!exists("QID.dat")){return(message("Warning: QID.dat does not exist"))}
if(!exists("Labels")){return(message("Warning: Labels does not exist"))}

for(n in 1:length(QID.dat)){
#n <- 1
message(n)
#head(QID.dat[[1]]); head(Labels)
Lab <- names(QID.dat)[n]; Lab00 <- paste(unlist(Labels[Labels$Subject == Lab & Labels$Property == "rdfs:label",c("Object", "Subject")]), collapse = ".")
Lab00

#Merge labels
QID.datR <- MergeWikiData(Data=QID.dat[[n]],
                          Labels=Labels)
QID.datR <- QID.datR[QID.datR$subject != QID.datR$parentClass,]
QID.datR$subjectLabel <- paste0(QID.datR$subjectLabel, ".", QID.datR$subject)
QID.datR$parentClassLabel <- paste0(QID.datR$parentClassLabel, ".", QID.datR$parentClass)
head(QID.datR)

agGraphSearch:::agVisNetwork_cr(Graph=QID.datR,
                                NodeColorRandom=F, Count=2,
                                Size=10, SmallSize=5, StarSize=10,
                                FontSize=7, HeightSclale = "750px",
                                WidthSclale = "110%", SEED=123, Selected=Lab00,
                                Browse=F, output=T, file=paste0("./agVisNetwork_", formatC(n, flag="0", width=4), "_", Lab00, ".html"))
suppressMessages(filesstrings::file.move(files = paste0("./agVisNetwork_", formatC(n, flag="0", width=4), "_", Lab00, ".html"),
                        destinations = "./04_Wiki_PolyInfo_01", overwrite = T))

}

}

#Calculate the common upper entities

calcCU <- function(){

if(!exists("QID.dat")){return(message("Warning: QID.dat does not exist"))}

QID.dat_02 <- c()
CUcounts00 <- c()

for(n in seq_len(length(QID.dat))){
  #n <- 1
  a <- QID.dat[[n]]
  a$triple <- paste0(a$Subject, ".", a$Property, ".", a$Object)
  b <- a[as.numeric(rownames(unique(a["triple"]))),]

  #Remove loops
  b <- b[b$Subject != b$Object,]
  QID.dat_02 <- QID.dat_02 %>% rbind(b)

  #CU counts
  CUcounts00 <- c(CUcounts00, unique(b$Object))
}

return(list(QID.dat=QID.dat_02,
            CUcounts=CUcounts00))

}

#Merge datasets for Graph analysis algorism

MeargeDatasets <- function(){

if(!exists("QID.dat")){return(message("Warning: QID.dat does not exist"))}
if(!exists("Labels")){return(message("Warning: Labels does not exist"))}
if(!exists("cu_table3")){return(message("Warning: cu_table3 does not exist"))}
if(!exists("QID.list")){return(message("Warning: QID.list does not exist"))}


QID.datRR <- NULL

for(n in 1:length(QID.dat)){
  #n <- 1
  message(n)
  datR <- MergeWikiData(Data=QID.dat[[n]],
                        Labels=Labels)
  print(head(datR))
  QID.datRR[[n]] <- datR
  names(QID.datRR)[n] <- names(QID.dat)[n]
}

#create list
graph_Dat <- list(graph_List = QID.datRR,
                  search_List = QID.list,
                  CU_List = cu_table3$parentClass)

return(graph_Dat)

}

# Subordinate search
subordinateSearch <- function(){

if(!exists("SearchRnage01")){return(message("Warning: SearchRnage01 does not exist"))}

ECU.list <- SearchRnage01

#Run
for(n in 1:nrow(ECU.list)){
#n <- 1
message("No.", n, ": ", ECU.list$CommonEntity[n], " ", ECU.list$Object[n])
Dat <- c(); x <- 0; Query <- ECU.list$CommonEntity[n]

repeat{
x <- x + 1
message("x: ", x)

#Both wdt:P279 and wdt:P31
b1 <- ClassHi[ClassHi$Object %in% Query,]

#Only wdt:P279 when x = Levels
if(x != ECU.list$Levels[n]){
  b1 <- b1[b1$Property == "wdt:P279",]
}

if(nrow(b1) == 0){
  rownames(Dat) <- 1:nrow(Dat)
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Property, ".", Dat$Object)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),-c(ncol(Dat))]
  rownames(Dat) <- 1:nrow(Dat)
  #head(Dat); dim(Dat)
  break
  }
Dat <- rbind(Dat, b1)
Query <- b1$Subject
if(x == ECU.list$Levels[n]){
  rownames(Dat) <- 1:nrow(Dat)
  Dat$triple <- paste0(Dat$Subject, ".", Dat$Property, ".", Dat$Object)
  Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),-c(ncol(Dat))]
  rownames(Dat) <- 1:nrow(Dat)
  #head(Dat); dim(Dat)
  break
}
}
#head(Dat); dim(Dat)
print(dim(Dat))
saveRDS(Dat,
        paste0("./04_Wiki_PolyInfo_03/",
               formatC(n, width = 4, flag = "0"),
                "_L", ECU.list$Levels[n], "_",
               ECU.list$CommonEntity[n],
               ".Rds"))
}
}

## Analyze the results
mergeECUsearch <- function(){

if(!exists("SearchRnage01")){return(message("Warning: SearchRnage01 does not exist"))}

ECU.list <- SearchRnage01
ECU.list$UniTripleNum <- NA
ECU.list$UniEntityNum <- NA
TotalData <- NULL

for(n in 1:nrow(ECU.list)){
#n <- 1
message(n)
b <- readRDS(paste0("./04_Wiki_PolyInfo_03/",
                    formatC(n, width = 4, flag = "0"),
                    "_L", ECU.list$Levels[n], "_",
                    ECU.list$CommonEntity[n], ".Rds"))

#head(b)
b$triple <- paste0(b$Subject, ".",
                    b$Property, ".",
                    b$Object)
ECU.list$UniTripleNum[n] <- length(unique(b$triple))
ECU.list$UniEntityNum[n] <- length(unique(c(b$Subject, b$Object)))
#head(ECU.list)
#head(b); head(b[,-4])
TotalData[[n]] <- b
}

#str(TotalData)
saveRDS(TotalData, "./04_Wiki_PolyInfo_XX/TotalData.Rds")
saveRDS(ECU.list, "./04_Wiki_PolyInfo_XX/ECU.list.Rds")
readr::write_excel_csv(ECU.list, "./04_Wiki_PolyInfo_XX/ECU.list.csv", append=F, col_names=T)

##step 01
head(ECU.list)
rownames(ECU.list) <- 1:nrow(ECU.list)
#str(TotalData)
TotalDataECU <- NULL
k <- max(ECU.list$Levels)

for(m in 1:k){
  #m <- 1
  message("m: ", m)
  d <- ECU.list[ECU.list$Levels == m,]
  d1 <- as.numeric(rownames(d))
  temp <- NULL
  #file.remove(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, ".csv"))
  system(paste0("rm -rf ./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, ".csv"))
  for(n in d1){
    #n <- 1
    message("n: ", n)
    readr::write_csv(TotalData[[n]],
                     paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, ".csv"),
                     append=T, col_names=F)
}
temp <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, ".csv"),
                        show_col_types = FALSE, col_names=F))
colnames(temp) <- c("Subject", "Property", "Object", "triple")
rownames(temp) <- 1:nrow(temp)
temp <- temp[as.numeric(rownames(unique(temp["triple"]))),]
#head(temp)
readr::write_csv(temp, paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, "R.csv"),
                 append=F, col_names=F)
}

#Remove
if(length(dir(path = "./04_Wiki_PolyInfo_06", pattern = "R_Acc.csv")) != 0){
  system("rm -rf TotalData_OutTriples_L*R_Acc.csv")
}

for(m in 1:k){
#m <- 1
message("m: ", m)
temp <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", m, "R.csv"),
                        show_col_types = FALSE, col_names=F))
for(l in m:k){
#l <- 1
message("l: ", l)
readr::write_csv(temp,
                 paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", l, "R_Acc.csv"),
                 append=T, col_names=F)
}}

}

#Evaluation for All candidates
EvaluationAll <- function(){

if(!exists("SearchRnage01")){return(message("Warning: SearchRnage01 does not exist"))}
if(!exists("OCR.eva")){return(message("Warning: OCR.eva does not exist"))}
if(!exists("CountDat")){return(message("Warning: CountDat does not exist"))}
if(!exists("ECU.list")){return(message("Warning: ECU.list does not exist"))}
k <- max(ECU.list$Levels)

for(n in 1:k){
  #n <- 1
  message("n: ", n)
  #Uni
  temp <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", n, "R.csv"),
                                     show_col_types = FALSE, col_names=F))
  colnames(temp) <- c("Subject", "Property", "Object", "triple")
  rownames(temp) <- 1:nrow(temp)
  temp <- temp[as.numeric(rownames(unique(temp["triple"]))),]
  #head(temp)
  message("Uni")
  CountDat$UniEntityNum[n]  <- length(unique(c(temp$Subject, temp$Object)))
  CountDat$UniTripleNum[n] <- length(unique(temp$triple))

  #Jpn
  UniEntity <- unique(c(temp$Subject, temp$Object))
  #head(Labels)
  LabelsNum <- Labels[Labels$Subject %in% UniEntity, ]
  CountDat$UniJpnLab[n] <- length(unique(LabelsNum$Subject))

  #Acc
  temp <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", n, "R_Acc.csv"),
                                     show_col_types = FALSE, col_names=F))
  colnames(temp) <- c("Subject", "Property", "Object", "triple")
  rownames(temp) <- 1:nrow(temp)
  temp <- temp[as.numeric(rownames(unique(temp["triple"]))),]

  #head(temp)
  message("Acc")
  CountDat$AccEntityNum[n]  <- length(unique(c(temp$Subject, temp$Object)))
  CountDat$AccTripleNum[n] <- length(unique(temp$triple))

  #Jpn
  UniEntity <- unique(c(temp$Subject, temp$Object))
  #head(Labels)
  LabelsNum <- Labels[Labels$Subject %in% UniEntity, ]
  CountDat$AccJpnLab[n] <- length(unique(LabelsNum$Subject))

  message("OCR")
  Eva00 <- unique(c(temp$Subject, temp$Object))
  #head(Labels)
  Eva01 <- unique(Labels[Labels$Subject %in% Eva00,]$Object)
  #Eva01
  Eva02 <- round(sum(Eva01 %in% OCR.eva)/length(OCR.eva)*100, 3)
  CountDat$ExtractedNum[n] <- sum(Eva01 %in% OCR.eva)
  CountDat$RateAccEntity[n] <- Eva02
  CountDat$TotalAnswerNum[n] <- length(OCR.eva)

  print(CountDat)

}

SearchRnage02 <- data.frame(No=names(table(SearchRnage01$Levels)),
each_entity=as.numeric(table(SearchRnage01$Levels)))
head(SearchRnage02)

#ECU
CountDat$RateAccEntityPrec <- round((CountDat$ExtractedNum / CountDat$AccEntityNum)*100, 5)
CountDat$RateAccEntityPrecLog <- round(logb((CountDat$ExtractedNum / CountDat$AccEntityNum)*100), 5)
CountDat$RateAccJpnPrec <- round((CountDat$ExtractedNum / CountDat$AccJpnLab)*100, 5)
CountDat$RateAccJpnPrecLog <- round(logb((CountDat$ExtractedNum / CountDat$AccJpnLab)*100), 5)

CountDat$each_entity <- SearchRnage02$each_entity
CountDat$sum_entity <- cumsum(SearchRnage02$each_entity)

readr::write_csv(CountDat, "./04_Wiki_PolyInfo_XX/CountDat.csv", append=F, col_names=T)

return(CountDat)

}


#Refinement: get the direction of the search entity
runRefinement <- function(){

if(!exists("SearchRnage01")){return(message("Warning: SearchRnage01 does not exist"))}
if(!exists("CU_List")){return(message("Warning: CU_List does not exist"))}
if(!exists("QID.id")){return(message("Warning: QID.id does not exist"))}
b1 <- SearchRnage01

for(n in 1:nrow(b1)){
#n <- 1
print(n)
d <- readRDS(paste0("./04_Wiki_PolyInfo_03/",
                    formatC(n, width = 4, flag = "0"),
                    "_L", b1$Levels[n], "_",
                    b1$CommonEntity[n], ".Rds"))
head(d)
message("All: ", dim(d)[1])

g <- simplify(graph_from_edgelist(as.matrix(d[,c(1,3)]), directed = T))
V(g)$color <- "lightyellow"

f1 <- names(V(g)) %in% unique(QID.id)
V(g)$color[f1] <- "skyblue"

f2 <- names(V(g)) %in% unique(b1$CommonEntity)
V(g)$color[f2] <- "pink"

f3 <- names(V(g)) %in% CU_List[!(CU_List %in% unique(b1$CommonEntity))]
V(g)$color[f3] <- "lightgreen"

f <- names(V(g)) %in% b1$CommonEntity[n]
V(g)$color[f] <- "blue"

len <- length(V(g)$color)
len1 <- length(V(g)$color[f1])
len2 <- length(V(g)$color[f2])
len3 <- length(V(g)$color[f3])

if(length(names(V(g))) < 100){
V(g)$size <- 3
V(g)$size[f] <- 6
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.75
V(g)$label.cex <- 0.1
CEX=0.2
}else{
if(length(names(V(g))) < 500){
V(g)$size <- 2
V(g)$size[f] <- 5
V(g)$size[f1] <- 3
V(g)$size[f2] <- 3
E(g)$width <- 0.5
V(g)$label.cex <- 0.05
CEX=0.1
}else{
V(g)$size <- 0.5
V(g)$size[f] <- 3
V(g)$size[f1] <- 1.5
V(g)$size[f2] <- 1.5
E(g)$width <- 0.1
V(g)$label.cex <- 0
CEX=0.01
}}

E(g)$lty <- 1

##########################################
#Pathway
##########################################
h1 <- c(1:length(names(V(g))))[names(V(g)) %in% b1$CommonEntity[n]]
h2 <- c(1:length(names(V(g))))[names(V(g)) %in% QID.d]
h2 <- h2[!c(h2 %in% h1)]

i <- c()
if(identical(h2, integer(0))){
i[[1]] <- NA
}else{
for(m in 1:length(h2)){
#m <- 1
suppressMessages(h3 <- names(get.shortest.paths(g,
                                                from=h1,
                                                to=h2[m],
                                                mode = "in")$vpath[[1]]))
if(length(h3) == 2){
if(b1$Levels[n] == 1){
i[[m]] <- NA
}else{
#low-level entities from search entity
i[[m]] <- h3
}
}else{
i[[m]] <- c(h3)
}
}
}

if(all(is.na(unlist(i)))){
V(g)$color[V(g)$color == "lightyellow"] <- "firebrick2"
saveRDS(d,
        paste0("./04_Wiki_PolyInfo_05/",
               formatC(n, width = 4, flag = "0"),
               "_L", b1$Levels[n], "_",
               b1$CommonEntity[n], "_refine.Rds"))
message("No Refine: ", dim(d)[1])

}else{
ii <- c()
for(l in 1:length(i)){
#l <- 1
#l <- 2
if(!all(is.na(unlist(i[[l]])))){
if(length(i[[l]]) == 2){
i0 <- i[[l]]
for(k in 1:length(i0)){
#k <- 1
if(i0[k] == b1$CommonEntity[n]){
  ii <- c(ii, names(neighbors(g, v=b1$CommonEntity[n], mode = "in")))
}else{
  ii <- c(ii, names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) %in% i0[k]], "in")))
}}
}else{
i0 <- i[[l]]
i0 <- i0[!c(i0 %in% b1$CommonEntity[n])]

if(length(i0) < 4){
for(k in 1:length(i0)){
#k <- 1
ii <- c(ii, names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) %in% i0[k]], "in")))
}
}else{
kk <- length(i0) - 2
for(k in kk:length(i0)){
#k <- 1
ii <- c(ii, names(subcomponent(g, c(1:length(names(V(g))))[names(V(g)) %in% i0[k]], "in")))
}
}
}
}else{
ii <- c(ii, names(neighbors(g, v=b1$CommonEntity[n], mode = "in")))
}
}

ii <- ii[!(ii %in% b1$CommonEntity[n])]
ii <- ii[!(ii %in% QID.d)]
V(g)$color[names(V(g)) %in% unique(ii)] <- "firebrick2"

#head(d)
Ex <- unique(names(V(g))[V(g)$color == "lightyellow"])

#head(d)
d1 <- d[!(d$Subject %in% Ex),]
d1 <- d1[!(d1$Object %in% Ex),]
#dim(d1)
saveRDS(d1,
        paste0("./04_Wiki_PolyInfo_05/",
               formatC(n, width = 4, flag = "0"),
               "_L", b1$Levels[n], "_",
               b1$CommonEntity[n], "_refine.Rds"))
message("Refine: ", dim(d1)[1])
}

if(nrow(d) < 100000){
##plot 1
set.seed(123)
system.time(layout03 <- layout_with_fr(g))
el <- data.table::as.data.table(as_edgelist(g)) %>%
  data.table::setnames(c("x","y"))

d_layout <- data.table::data.table(x_coord = layout03[,1], y_coord = layout03[,2], id = names(V(g)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

#head(d_layout)
if(as.vector(Sys.info()["sysname"] == "Darwin")){quartz(width=7.5, height=7.5)}
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA,
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
segments(x0  = el_w_layout$x1, x   = el_w_layout$x2,
         y0  = el_w_layout$y1, y   = el_w_layout$y2,
         col = scales::alpha("grey", .3))
points(d_layout$x_coord, d_layout$y_coord, pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color,
       main=paste0( b1$Label.j[n], "\nTotal: ", len, ", Search: ", len1, ", Common: ", len2, ", both: ", len3))
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)
}
if(as.vector(Sys.info()["sysname"] == "Darwin")){
quartz.save(file = paste0("./04_Wiki_PolyInfo_05/",
            formatC(n, width = 4, flag = "0"),
            "_L", b1$Levels[n], "_",
            b1$CommonEntity[n], "_refine1.png"),
            type = "png", dpi=300); dev.off()}
}

##plot 2
if(!all(is.na(unlist(i)))){
g <- simplify(graph_from_edgelist(as.matrix(d1[,c(1,3)]), directed = T))
V(g)$color <- "lightyellow"

f1 <- names(V(g)) %in% unique(QID.d)
V(g)$color[f1] <- "skyblue"

f2 <- names(V(g)) %in% unique(b1$CommonEntity)
V(g)$color[f2] <- "pink"

f3 <- names(V(g)) %in% CU_List[!(CU_List %in% unique(b1$CommonEntity))]
V(g)$color[f3] <- "lightgreen"

f <- names(V(g)) %in% b1$CommonEntity[n]
V(g)$color[f] <- "blue"

V(g)$color[names(V(g)) %in% unique(ii)] <- "firebrick2"

len <- length(V(g)$color)
len1 <- length(V(g)$color[f1])
len2 <- length(V(g)$color[f2])
len3 <- length(V(g)$color[f3])

if(length(names(V(g))) < 100){
V(g)$size <- 3
V(g)$size[f] <- 6
V(g)$size[f1] <- 4
V(g)$size[f2] <- 4
E(g)$width <- 0.75
V(g)$label.cex <- 0.1
CEX=0.1
}else{
if(length(names(V(g))) < 500){
V(g)$size <- 2
V(g)$size[f] <- 5
V(g)$size[f1] <- 3
V(g)$size[f2] <- 3
E(g)$width <- 0.5
V(g)$label.cex <- 0.05
CEX=0.05
}else{
V(g)$size <- 0.5
V(g)$size[f] <- 3
V(g)$size[f1] <- 1.5
V(g)$size[f2] <- 1.5
E(g)$width <- 0.1
V(g)$label.cex <- 0
CEX=0.01
}}

E(g)$lty <- 1

if(nrow(d) < 100000){
set.seed(123)
system.time(layout03 <- layout_with_fr(g))
el <- data.table::as.data.table(as_edgelist(g)) %>%
  data.table::setnames(c("x","y"))

d_layout <- data.table::data.table(x_coord = layout03[,1], y_coord = layout03[,2], id = names(V(g)))
el_w_layout <- merge(el, d_layout, by.x = "x", by.y = "id", all.x = TRUE) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x1", "y1") ) %>%
        merge( d_layout, by.x = "y", by.y = "id", all.x = TRUE ) %>%
        data.table::setnames( c("x_coord", "y_coord"), c("x2", "y2") )

head(d_layout)
if(as.vector(Sys.info()["sysname"] == "Darwin")){quartz(width=7.5, height=7.5)}
par(bg="white", cex.main=0.5, family="HiraKakuProN-W3", mgp=c(2.5, 1, 0), mai=c(0.5, 0.5, 0.5, 0.5))
plot(d_layout[,1:2], axes = F, type = "n", xlab = NA, ylab = NA,
  xlim=c(c(range(d_layout$x_coord)) + c(-diff(range(d_layout$x_coord))*0.05, diff(range(d_layout$x_coord))*0.05)),
  ylim=c(c(range(d_layout$y_coord)) + c(-diff(range(d_layout$y_coord))*0.05, diff(range(d_layout$y_coord))*0.05)))
segments(x0  = el_w_layout$x1, x   = el_w_layout$x2,
         y0  = el_w_layout$y1, y   = el_w_layout$y2,
         col = scales::alpha("grey", .3))
points(d_layout$x_coord, d_layout$y_coord, pch = 21, col="grey", lwd=0.2, cex = V(g)$size, bg=V(g)$color,
       main=paste0( b1$Label.j[n], "\nTotal: ", len, ", Search: ", len1, ", Common: ", len2, ", both: ", len3))
if(len < 10000){
text(d_layout$x_coord, d_layout$y_coord, labels=d_layout$id, cex=CEX)
}
if(as.vector(Sys.info()["sysname"] == "Darwin")){
quartz.save(file = paste0("./04_Wiki_PolyInfo_05/",
            formatC(n, width = 4, flag = "0"),
            "_L", b1$Levels[n], "_",
            b1$CommonEntity[n], "_refine2.png"),
            type = "png", dpi=300); dev.off()}
}



#Num
d.len <- length(unique(c(d$Subject, d$Object)))
d1.len <- length(unique(c(d1$Subject, d1$Object)))
d2 <- paste0("Delete: ", d.len, " => ", d1.len)

#
readr::write_csv(data.frame(d2),
                file = paste0("./04_Wiki_PolyInfo_05/",
                              formatC(n, width = 4, flag = "0"),
                              "_L", b1$Levels[n], "_",
                              b1$CommonEntity[n], "_Delete.csv"),
                append=F, col_names=F)

#Save Delete_all.csv
readr::write_csv(data.frame(X=paste0("No: ", n), Y=d2),
                file = paste0("./04_Wiki_PolyInfo_05/Delete_all.csv"),
                append=T, col_names=F)

}
}
}

#Read All candidates
convRDF_all <- function(N,
                        FileName="./04_Wiki_PolyInfo_07/RDF_from_Wikidat_all_v02.nt"){

if(!exists("Labels")){return(message("Warning: Labels does not exist"))}
if(!exists("QID.list")){return(message("Warning: QID.list does not exist"))}

message("read_csv")
Dat <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/TotalData_OutTriples_L", N, "R_Acc.csv"),
                                  show_col_types = FALSE, col_names=F))
colnames(Dat) <- c("Subject", "Property", "Object", "triple")
rownames(Dat) <- 1:nrow(Dat)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
#length(unique(c(Dat$Subject, Dat$Object)))

#remove the column of triple
head(Dat)
Dat00 <- Dat[,-4]
head(Dat00)

#Save
#readr::write_csv(Dat00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_all_class.csv", col_names=F)

#Class
message("Class")
nt <- paste0("<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Subject), "> ",
            "<http://www.w3.org/2000/01/rdf-schema#subClassOf> ",
            "<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Object), "> .")
nt <- unique(nt)
write.table(nt, file=FileName, sep="",
            row.names = F, col.names = F, append=F, quote = F)

#wdt:P31 or wdt:P279
message("wdt:P31 or wdt:P279")
head(Dat00)
nt <- paste0("<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Subject), "> ",
            "<", sub("wdt[:]", "http://www.wikidata.org/prop/direct/", Dat00$Property), "> ",
            "<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Object), "> .")
nt <- unique(nt)
head(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#Search entity
message("Search entity")
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", QID.list), '> ',
            '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://class/search> .')
nt <- unique(nt)
head(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#Jpn Labels
message("Jpn Labels")
Dat00entity <- unique(c(Dat00$Subject, Dat00$Object))
head(Dat00entity)

Labels <- readRDS("./04_Wiki_PolyInfo_00/Label_ja_All_label_df.Rds")
head(Labels)
Labels00 <- Labels[Labels$Subject %in% Dat00entity,]
head(Labels00)
#table(Labels00$Property)
#Save
#readr::write_csv(Labels00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_all_labels_ja.csv", col_names=F)

#rdfs:label
cc <- grepl("rdfs:label", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', Labels00$Object[cc], '"@ja .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#skos:altLabel
cc <- grepl("skos:altLabel", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2004/02/skos/core#altLabel> ',
            '"', Labels00$Object[cc], '"@ja .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#En Labels
message("En Labels")
Labels <- readRDS("./04_Wiki_PolyInfo_00/Label_en_All.Rds")
head(Labels)
Labels00 <- Labels[Labels$Subject %in% Dat00entity,]
head(Labels00)
#table(Labels00$Property)

#Save
#readr::write_csv(Labels00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_all_labels_en.csv", col_names=F)

#rdfs:label
cc <- grepl("rdfs:label", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', Labels00$Object[cc], '"@en .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#skos:altLabel
cc <- grepl("skos:altLabel", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2004/02/skos/core#altLabel> ',
            '"', Labels00$Object[cc], '"@en .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

message("Finished!!")

}

#convert data after refinement
convRDF_refinement <- function(N,
                               FileName){
if(!exists("Labels")){return(message("Warning: Labels does not exist"))}
if(!exists("QID.list")){return(message("Warning: QID.list does not exist"))}

message("read_csv")
Dat <- data.frame(readr::read_csv(paste0("./04_Wiki_PolyInfo_06/Refinement_OutTriples_L", N, "R_Acc.csv"),
                                  show_col_types = FALSE, col_names=F))
colnames(Dat) <- c("Subject", "Property", "Object", "triple")
rownames(Dat) <- 1:nrow(Dat)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
#length(unique(c(Dat$Subject, Dat$Object)))

#remove the column of triple
head(Dat)
Dat00 <- Dat[,-4]
head(Dat00)

#Save
#readr::write_csv(Dat00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_refi_class.csv", col_names=F)

#Class
message("Class")
nt <- paste0("<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Subject), "> ",
            "<http://www.w3.org/2000/01/rdf-schema#subClassOf> ",
            "<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Object), "> .")
nt <- unique(nt)
write.table(nt, file=FileName, sep="",
            row.names = F, col.names = F, append=F, quote = F)

#wdt:P31 or wdt:P279
message("wdt:P31 or wdt:P279")
head(Dat00)
nt <- paste0("<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Subject), "> ",
            "<", sub("wdt[:]", "http://www.wikidata.org/prop/direct/", Dat00$Property), "> ",
            "<", sub("wd[:]", "http://www.wikidata.org/entity/", Dat00$Object), "> .")
nt <- unique(nt)
head(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#Search entity
message("Search entity")
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", QID.list), '> ',
            '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://class/search> .')
nt <- unique(nt)
head(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#Jpn Labels
message("Jpn Labels")
Dat00entity <- unique(c(Dat00$Subject, Dat00$Object))
head(Dat00entity)

Labels <- readRDS("./04_Wiki_PolyInfo_00/Label_ja_All_label_df.Rds")
head(Labels)
Labels00 <- Labels[Labels$Subject %in% Dat00entity,]
head(Labels00)
#table(Labels00$Property)
#Save
#readr::write_csv(Labels00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_refi_labels_ja.csv", col_names=F)

#rdfs:label
cc <- grepl("rdfs:label", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', Labels00$Object[cc], '"@ja .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#skos:altLabel
cc <- grepl("skos:altLabel", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2004/02/skos/core#altLabel> ',
            '"', Labels00$Object[cc], '"@ja .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#En Labels
message("En Labels")
Labels <- readRDS("./04_Wiki_PolyInfo_00/Label_en_All.Rds")
head(Labels)
Labels00 <- Labels[Labels$Subject %in% Dat00entity,]
head(Labels00)
#table(Labels00$Property)

#Save
#readr::write_csv(Labels00, "./04_Wiki_PolyInfo_07/Wikidata_Ont12_refi_labels_en.csv", col_names=F)

#rdfs:label
cc <- grepl("rdfs:label", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', Labels00$Object[cc], '"@en .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#skos:altLabel
cc <- grepl("skos:altLabel", Labels00$Property)
#table(cc)
nt <- paste0('<', sub("wd[:]", "http://www.wikidata.org/entity/", Labels00$Subject[cc]), '> ',
            '<http://www.w3.org/2004/02/skos/core#altLabel> ',
            '"', Labels00$Object[cc], '"@en .')
nt <- unique(nt)
head(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

message("Finished!!")

}








