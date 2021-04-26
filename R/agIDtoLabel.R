##' @title Searching the label corresponding to the entity url via SPARQL.
##'
##' @param Entity_ID a character vector corresponing to the entity ID.
##' @param EndPoint a string of SPARQL endpoint. ex. http://....
##' @param FROM a string of graph URI in the endpoint. The default is blank ("").
##' @param PropertyForLabel a numeric vector.
##' if the number is 1 (default), the label relation use "rdfs:label".
##' if the number is 2, the label relation use " skos:prefLabel".
##' @param lang a numeric vector.
##' if the number is 1 (default), the label contains English label (@en).
##' if the number is 2, the label contains Japanese label (@ja).
##' if the number is 3, the labels contain both English and Japanese labels.
##'
##' @param Message logical; perform an output of Entity_ID or not.
##'
##' @description this function is a general function for
##' searching the RDF data using an entity URI via SPARQL.
##'
##' @return data.frame
##' @author Satoshi Kume
##' @export agIDtoLabel
##' @examples \dontrun{
##' #parameters
##'
##' #polymer (wikidata prefix URI: wd:Q81163)
##' ID <- "wd:Q81163"
##'
##' print(KzLabEndPoint)
##' print(wikidataClassProperty)
##'
##' #run
##' agIDtoLabel(
##'   Entity_ID=ID,
##'   EndPoint=KzLabEndPoint$EndPoint,
##'   FROM=KzLabEndPoint$FROM
##'   )
##'
##' }

agIDtoLabel <- function(Entity_ID,
                        EndPoint,
                        FROM,
                        PropertyForLabel=1,
                        lang=1,
                        Message=TRUE){

#Parameters
ID <- Entity_ID

Prefix <- agGraphSearch:::PREFIX

switch(as.character(PropertyForLabel),
      "1" = Property <- "rdfs:label",
      "2" = Property <- "skos:prefLabel",
      return(message("Warning: Not  proper value of PropertyForLabel"))
)

Query <-paste('
SELECT distinct ?entityNamej ?entityNamee', ' ',
FROM, ' ',
'WHERE {
optional {', ID, ' ', Property, ' ?entityNamej. filter(LANG(?entityNamej) = "ja")}
optional {', ID, ' ', Property, ' ?entityNamee. filter(LANG(?entityNamee) = "en")}',
'}', sep="")

if(Message){message(paste("Query: ", ID, sep=""))}
A <- try(SPA <- SPARQL(url=EndPoint, query=paste(Prefix, Query))$results, silent = T)
if(class(A) == "try-error"){return(NULL)}

if(!exists("SPA")){ return(message("Perhaps No Internet Services")) }

try(SPA$entityNamej <- gsub("^\"", "", SPA$entityNamej), silent = T)
try(SPA$entityNamej <- gsub("\"@ja$", "", SPA$entityNamej), silent = T)
try(SPA$entityNamee <- gsub("^\"", "", SPA$entityNamee), silent = T)
try(SPA$entityNamee <- gsub("\"@en$", "", SPA$entityNamee), silent = T)

switch(as.character(lang),
      "1" = SPA00 <- SPA[,c(2)],
      "2" = SPA00 <- SPA[,c(1)],
      "3" = SPA00 <- SPA,
      return(message("Warning: Not  proper value of lang"))
)

return(data.frame(ID=ID, SPA00, stringsAsFactors = F))

}

