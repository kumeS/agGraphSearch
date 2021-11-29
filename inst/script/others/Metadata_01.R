rm(list=ls())
source("../00_R_Script_gist/SPARQL_Wikidata_Query_10R.R")

options(max.print=999999)
library(stringr)
library(purrr)
#install.packages("furrr")
library(furrr)
library(progress)
library(VennDiagram)
library(beepr)
library(plotly)

availableCores()
plan(multisession(workers = 4))
plan()

Label <- c("wd:Q2996394", "wd:Q5058355", "wd:Q70551253", "wd:Q14860489", "wd:Q196538")
Graph_u01 <- future_map(unlist(Label), wikiGraph_plusWD_v2_up, .progress = TRUE)

NN <- 5
Graph_00 <- Graph_u01[[NN]]
Graph_00$subjectLabel <- paste(Graph_00$subjectLabel, ".", Graph_00$subject, sep="") 
Graph_00$parentClassLabel <- paste(Graph_00$parentClassLabel, ".", Graph_00$parentClass, sep="") 
WikiVisNetwork(Graph=Graph_00, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
               SEED=123, Selected=NULL, Browse=FALSE, output=T, 
               file=paste("Wikidat_visNet_", NN,"_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE)


NNN <- 1
Graph_d01 <- wikiGraph_plusWD_v2_down(WD_EntityID=Label[NNN], Depth=3, 
                                Prop1="wdt:P279", Prop2="wdt:P279")
head(Graph_d01)
tail(Graph_d01)
table(is.na(Graph_d01[,c(2:4, 6)]))
table(is.na(Graph_d01$subjectLabel))
table(is.na(Graph_d01$subjectLabelj))
table(is.na(Graph_d01$subjectLabele))

apply(Graph_d01[,2:4], 1, function(x) {all(is.na(x))} )
table(apply(Graph_d01[,2:4], 1, function(x) {all(is.na(x))} ))
Graph_d01[c(apply(Graph_d01[,2:4], 1, function(x) {all(is.na(x))})),]

#Graph_d01[Graph_d01[,1] == "wd:Q2449090",]
#Graph_d01[Graph_d01[,5] == "wd:Q2449090",]
#Graph_d01[Graph_d01[,1] == "wd:Q11985298",]
#Graph_d01[Graph_d01[,5] == "wd:Q11985298",]
#Graph_d01[Graph_d01[,1] == "wd:Q25395884",]
#Graph_d01[Graph_d01[,5] == "wd:Q25395884",]

#入れ替え
Graph_d01[c(apply(Graph_d01[,2:4], 1, function(x) {all(is.na(x))})), c(2)] <- Graph_d01[c(apply(Graph_d01[,2:4], 1, function(x) {all(is.na(x))})), c(1)]

Graph_00 <- Graph_d01
Graph_00$subjectLabel <- paste(Graph_00$subjectLabel, ".", Graph_00$subject, sep="") 
Graph_00$childClassLabel <- paste(Graph_00$childClassLabel, ".", Graph_00$childClass, sep="") 
WikiVisNetwork(Graph=Graph_00, NodeColorRandom=F, Count=2, Size=10, SmallSize=5, StarSize=10, FontSize=7, HeightSclale = "750px", WidthSclale = "110%",
               SEED=123, Selected=NULL, Browse=FALSE, output=T, 
               file=paste("Wikidat_visNet_Down_", NN,"_", format(Sys.time(), "%y%m%d_%H%M"),".html", sep=""), outputNodesEdges=FALSE)





