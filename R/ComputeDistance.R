##' @title Compute distance metrics between input strings and rdfs
##'
##' @param InputTerms a vector of character; a target list of words.
##' @param RDFterms a vector of character.
##' @param q Size of the q-gram
##' @param nthread Maximum number of threads to use.
##' @param TopWords Number of words with top strings distances
##'
##' @description This function counts the particular relations between strings.
##' Perfect Match, partial Match, strings similarity such as
##' longest common substring distance (lcs) and
##' cosine distance between q-gram profiles (cosine, N=2).
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export ComputeDistance
##' @importFrom stringdist stringdist
##' @examples \dontrun{
##'
##' InputTerms <- "polymer"
##' RDFterms <- "Polymers"
##'
##' ComputeDistance(
##'   InputTerms,
##'   RDFterms
##'   )
##'
##' }

ComputeDistance <- function(InputTerms,
                            RDFterms,
                            q=2,
                            nthread=4,
                            TopWords=3){

if(!is.vector(InputTerms)){ return(message("Warning: No proper value of InputTerms")) }
if(!is.vector(RDFterms)){ return(message("Warning: No proper value of RDFterms")) }

InputTerms01 <- InputTerms
RDFterms01 <- RDFterms

#Counting table
Dat <- data.frame(X1=InputTerms01,
                  X2=NA, X3=NA,
                  X4=NA, X5=NA, X6=NA,
                  X7=NA, X8=NA, X9=NA)

for(m in seq_len(length(InputTerms01))){
#m <- 1
message(paste0("No.: ", m))
message(paste0("Term: ", InputTerms01[m]))

#Perfect Match
cc <- grepl(pattern = paste0("^", InputTerms01[m], "$"),
            RDFterms01)
if(any(cc)){
  Dat01 <- RDFterms01[cc]
  Dat$X2[m] <- length(Dat01)
}else{
  Dat$X2[m] <- 0
}
message(paste0("PerfectMatch: ", Dat$X2[m]))

#Partial Match
cc <- grepl(pattern = InputTerms01[m],
            RDFterms01)
if(any(cc)){
  Dat01 <- RDFterms01[cc]
  Dat$X3[m] <- length(Dat01)
}else{
  Dat$X3[m] <- 0
}
message(paste0("Partial Match: ", Dat$X3[m]))

#Compute distance metrics between strings
#lcs
cc <- stringdist::stringdist(InputTerms01[m], RDFterms01, method="lcs", nthread=nthread)
if(length(cc) < TopWords){
  dd <- RDFterms01[order(cc)][1:length(cc)]
}else{
  dd <- RDFterms01[order(cc)][1:TopWords]
}

Dat$X4[m] <- length(dd)
Dat$X5[m] <- paste(dd, collapse = "; ")
Dat$X6[m] <- round(min(cc), 4)
message(paste0("Hits by lcs: ", Dat$X4[m]))
message(paste0("Words by lcs: ", Dat$X5[m]))
message(paste0("min: ", Dat$X6[m]))

#cosine
cc <- stringdist::stringdist(InputTerms01[m], RDFterms01, method="cosine", nthread=nthread, q=q)
if(length(cc) < TopWords){
  dd <- RDFterms01[order(cc)][1:length(cc)]
}else{
  dd <- RDFterms01[order(cc)][1:TopWords]
}
Dat$X7[m] <- length(dd)
Dat$X8[m] <- paste(dd, collapse = "; ")
Dat$X9[m] <- round(min(cc), 4)
message(paste0("Hits by cosine: ", Dat$X7[m]))
message(paste0("Words by cosine: ", Dat$X8[m]))
message(paste0("min: ", Dat$X9[m]))
}

#rename
colnames(Dat) <- c("Terms", "PerfectMatch", "PartialMatch",
                   "lcs1", "lcs2", "lcs3",
                   "cosine1", "cosine2", "cosine3")
#head(Dat)
return(Dat)

}






