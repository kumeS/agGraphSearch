
searchSuperordinate <- function(a2=a2, ChEBIrelation=ChEBIrelation){
Dat <- c(); x <- 1; Query <- a2

repeat{
message(x)
b1 <- ChEBIrelation[ChEBIrelation$Subject %in% Query,]
if(dim(b1)[1] == 0){break}
Dat <- rbind(Dat, b1)
Query <- b1$Object
x <- x + 1
if(x == 100){break}
}

#Exclude duplicates
head(Dat); dim(Dat)
rownames(Dat) <- 1:nrow(Dat)
if(length(colnames(Dat)) == 3){
  colnames(Dat) <- c("subject", "property", "parentClass")
}
if(length(colnames(Dat)) == 6){
  colnames(Dat) <- c("subject", "property", "parentClass", "OtherInfo", "SubInfo", "ObjInfo")
}

Dat$triple <- paste0(Dat$subject, ".", Dat$property, ".", Dat$parentClass)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
Dat <- Dat[,-ncol(Dat)]

message("head(Dat)")
print(head(Dat))

message("dim(Dat)")
print(dim(Dat))

message("length(unique(c(Dat$subject, Dat$parentClass)))")
print(length(unique(c(Dat$subject, Dat$parentClass))))

return(Dat)

}


searchSubordinate <- function(a3=a3, ChEBIrelation=ChEBIrelation){

Dat <- c(); x <- 1; Query <- a3

repeat{
message(x)
b2 <- ChEBIrelation[ChEBIrelation$Object %in% Query,]
if(dim(b2)[1] == 0){break}
Dat <- rbind(Dat, b2)
Query <- b2$Subject
x <- x + 1
if(x == 100){break}
}

#Exclude duplicates
head(Dat); dim(Dat)
rownames(Dat) <- 1:nrow(Dat)
if(length(colnames(Dat)) == 3){
  colnames(Dat) <- c("subject", "property", "parentClass")
}
if(length(colnames(Dat)) == 6){
  colnames(Dat) <- c("subject", "property", "parentClass", "OtherInfo", "SubInfo", "ObjInfo")
}

Dat$triple <- paste0(Dat$subject, ".", Dat$property, ".", Dat$parentClass)
Dat <- Dat[as.numeric(rownames(unique(Dat["triple"]))),]
Dat <- Dat[,-ncol(Dat)]

message("head(Dat)")
print(head(Dat))

message("dim(Dat)")
print(dim(Dat))

message("length(unique(c(Dat$subject, Dat$parentClass)))")
print(length(unique(c(Dat$subject, Dat$parentClass))))

return(Dat)

}



