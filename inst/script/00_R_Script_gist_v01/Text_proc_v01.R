##Text search
Text_proc_up <- function(List=QID_rdfsLabel02, StopEntity="wd:Q35120", Data=a){

Output <- c()
Output01 <- c()
for(N in seq_len(nrow(List))){
#N <- 1
SearchLabel <- List[N,]
message(paste0("No.: ", N))
message(paste0("CommonEntity: ", SearchLabel$QID))
message(paste0("entityNamej: ", SearchLabel$entityNamej))
message(paste0("Hierarchy: ", SearchLabel$Levels))

#上位検索
Query <- SearchLabel$QID
x <- 0

repeat{
x <- x +1
message(paste0("up: ", x))
cc <- c(Data[,1] %in% Query)
bb <- Data[cc,]
#head(bb)

if(x != 1){
bb <- bb[bb[,2] != "wdt:P31",]  
}

if(nrow(bb) > 0){
Output01 <- Output01 %>% rbind(bb)
Query <- bb[,3]
if(any(bb$X3 %in% StopEntity)){break}
}else{ break }
}
Output[[N]] <- bb
message(paste0("Number: ", nrow(bb)))
}
}





