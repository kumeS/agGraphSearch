##' @title ComputeDistance: Compute distance metrics between input strings and rdfs
##'
##' @param InputTerms a vector of character; a target list of words.
##' @param RDFterms a vector of character.
##' @param q Size of the q-gram
##' @param nthread Maximum number of threads to use.
##' @param TopWords Number of words with top strings distances
##'
##' @description This function counts the particular relations between strings.
##' Perfect Match, partial Match, strings similarity using
##' cosine distance between q-gram profiles (cosine, N=2).
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export ComputeDistance
##' @importFrom stringdist stringdist
##' @importFrom magrittr %>%
##' @examples \dontrun{
##'
##' library(magrittr)
##'
##' InputTerms <- "polymer"
##' RDFterms <- "Polymers"
##'
##' ComputeDistance(
##'   InputTerms,
##'   RDFterms
##'   )
##' }
##'


ComputeDistance <- function(InputTerms,
                            RDFterms,
                            q=2,
                            nthread=4,
                            TopWords=3){

if(!is.vector(InputTerms)){ return(message("Warning: No proper value of InputTerms")) }
if(!is.vector(RDFterms)){ return(message("Warning: No proper value of RDFterms")) }

InputTerms01 <- InputTerms
RDFterms01 <- RDFterms

Dat0 <- c()
for(m in seq_len(length(InputTerms01))){
Dat <- data.frame(X1=rep(InputTerms01[m], TopWords),
                  X2=NA, X3=NA,
                  X4=NA, X5=NA, X6=NA)
#head(Dat)
message(paste0("No.: ", m))
message(paste0("Term: ", InputTerms01[m]))

#Perfect Match
cc <- grepl(pattern = paste0("^", InputTerms01[m], "$"),
            RDFterms01)
if(any(cc)){
  Dat01 <- RDFterms01[cc]
  Dat$X2[1] <- length(Dat01)
}else{
  Dat$X2[1] <- 0
}
message(paste0("PerfectMatch: ", Dat$X2[1]))

#Partial Match
cc <- grepl(pattern = InputTerms01[m],
            RDFterms01)
if(any(cc)){
  Dat01 <- RDFterms01[cc]
  Dat$X3[1] <- length(Dat01)
}else{
  Dat$X3[1] <- 0
}
message(paste0("Partial Match: ", Dat$X3[1]))

#Compute distance metrics between strings
#cosine
cc <- stringdist::stringdist(InputTerms01[m], RDFterms01, method="cosine", nthread=nthread, q=q)
if(length(cc) < TopWords){
  dd <- RDFterms01[order(cc)][1:length(cc)]
  Dat$X6 <- round(cc[order(cc)][1:length(cc)], 4)
}else{
  dd <- RDFterms01[order(cc)][1:TopWords]
  Dat$X6 <- round(cc[order(cc)][1:TopWords], 4)
}

Dat$X4 <- length(dd)
Dat$X5 <- dd

message(paste0("Hits by cosine: ", Dat$X4[1]))
message(paste0("Words by cosine: ", paste(Dat$X5, collapse = "; ")))
message(paste0("cosine distance: ", paste(Dat$X6, collapse = "; ")))

Dat0 <- Dat0 %>% rbind(Dat)
}

#rename
colnames(Dat0) <- c("Terms", "PerfectMatch", "PartialMatch",
                    "cosine1", "cosine2", "cosine3")

#head(Dat0)
return(Dat0)

}






