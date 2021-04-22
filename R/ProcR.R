#Bind lists to data.frame
ListDF2DF <- function(input){

if(!is.list(input)){return(message("Warning: Not proper value of input"))}

a <- c()
for(n in seq_len(length(input))){
a <- a %>% rbind(input[[n]])
}

return(data.frame(a, stringsAsFactors = F))

}

