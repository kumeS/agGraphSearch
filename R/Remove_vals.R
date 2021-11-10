##' @title Remove_vals: remove unnecessary strings for Japanese.
##'
##' @param Data a data frame with columns of "Subject", "Property", "Object", and "OtherInfo".
##' @param StringLength a numeric; upper limit of string length.
##'
##' @description Exclude one character, number rules and symbols.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export Remove_vals
##'

Remove_vals <- function(Data,
                        StringLength=30){
#Data = Labels00
if(!all(colnames(Data) == c("Subject", "Property", "Object", "OtherInfo"))){
  return(message("Warning: Not proper value of Data"))
}

#Exclude one character
words01 <- Data
words01$nchar <- nchar(words01$Object)
d <- words01[words01$nchar == 1, ]
words02 <- words01[words01$nchar != 1, ]

if(!is.null(StringLength)){
  if(is.numeric(StringLength)){
    d <- rbind(d, words02[words02$nchar > StringLength, ])
    words02 <-  words02[words02$nchar <= StringLength, ]
    print(table(words02$nchar))
  }
}

#Exclude number rules
cc <- grepl("^[0-9][0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9][0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]..[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]...[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]....[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].....[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]......[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].......[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]........[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].........[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]..........[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]...........[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]............[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].............[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]..............[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]...............[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]................[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9].................[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]..................[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9][0-9][0-9][0-9]...................[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]年", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]年代", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]月", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]日", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]時間", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]年度", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]世紀", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]歳", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]円", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]万", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]つ", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]番", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]丁", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]の", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]度", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]形", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9]等", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("第[0-9]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9].$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9]..$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[0-9]...$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^.[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^..[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^...[0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

#Exclude symbols
cc <- grepl("^[.]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[.]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[.]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[/][/]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[/][0-9]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[/][a-z]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[a-z][/]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[,]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[:]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[;]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[%]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[$]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[&]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[!]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[']$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("^[(]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[+]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[*]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[?]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[,][0-9]$", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[[]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[]]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("\"", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("\'", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[・][・]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

cc <- grepl("[0-9][・][0-9]", words02$Object)
d <- rbind(d, words02[cc, ])
words02　<- words02[!cc, ]

#Results
words03 <- words02[,-ncol(words02)]
rownames(words03) <- 1:nrow(words03)
rownames(d) <- 1:nrow(d)

words04 <- list(Results=words03,
                Exclude=d)
return(words04)


}
