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

