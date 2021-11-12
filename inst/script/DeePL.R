##' @title DeepL API on R
##'
##' @param Sentence an input string.
##' @param EN2JA a logical.
##' @param Auth_Key a string, Your Authentication Key
##'
##' @return character
##' @author Satoshi Kume
##' @export DeePL
##'

DeePL <- function(Sentence, EN2JA=TRUE, Auth_Key="Your Authentication Key"){

if(EN2JA){
a <- system(paste0('curl -s https://api-free.deepl.com/v2/translate -d "auth_key=', Auth_Key, '" -d "text=', Sentence, '" -d source_lang="EN" -d "target_lang=JA"'), intern = T)
}else{
a <- system(paste0('curl -s https://api-free.deepl.com/v2/translate -d "auth_key=', Auth_Key, '" -d "text=', Sentence, '" -d source_lang="JA" -d "target_lang=EN"'), intern = T)
}

result <- strsplit(strsplit(as.character(a), "\"text\":\"")[[1]][2], "\"}]}")[[1]][1]
return(result)

}

Trans2Jpn <- function(words02=words02){

words02$Jpn <- NA
words02$cosine2_Jpn <- NA

if(colnames(words02)[1] != "Terms"){return(message("warning: not proper value of words02"))}
if(colnames(words02)[5] != "cosine2"){return(message("warning: not proper value of words02"))}

wordsTerms <- unique(c(words02$Terms, words02$cosine2))

#head(words02)
#str(wordsTerms)
#length(wordsTerms)

for(n in 1:length(wordsTerms)){
#n <- 1
Result <- DeePL(Sentence=wordsTerms[n],
                Auth_Key="43725ffd-d3d3-a301-c92d-8d7e2070f71c:fx")

words02$Jpn[words02$Terms == wordsTerms[n]] <- Result
words02$cosine2_Jpn[words02$cosine2 == wordsTerms[n]] <- Result

message(colourise(paste("  No.: ", n, " / ", length(wordsTerms)), fg = "green", bg = NULL))
message(colourise(paste("  ", wordsTerms[n], "=>", Result), fg = "green", bg = NULL))
}

return(words02)

}

