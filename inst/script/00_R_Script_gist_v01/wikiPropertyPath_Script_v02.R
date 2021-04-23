
if(!require("WikidataQueryServiceR")){install.packages("WikidataQueryServiceR")}; library(WikidataQueryServiceR)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)
if(!require("networkD3")){install.packages("networkD3")}; library(networkD3)
if(!require("htmlwidgets")){install.packages("htmlwidgets")}; library(htmlwidgets)
if(!require("franc")){install.packages("franc")}; library(franc)
if(!require("visNetwork")){install.packages("visNetwork")}; library(visNetwork)
#if(!require("progress")){install.packages("progress")}; library(progress)
if(!require("DT")){install.packages("DT")}; library(DT)
if(!require("formattable")){install.packages("formattable")}; library(formattable)
if(!require("data.tree")){install.packages("data.tree")}; library(data.tree)
if(!require("htmltools")){install.packages("htmltools")}; library(htmltools)
if(!require("purrr")){install.packages("purrr")}; library(purrr)
if(!require("beepr")){install.packages("beepr")}; library(beepr)
if(!require("readr")){install.packages("readr")}; library(readr)

#QID_Name="wd:Q10913792"; PropertyPathDepth=1; lang=1; Upper=F;p1="wdt:P31|wdt:P279";p2="wdt:P279";p3="wdt:P279";p4="wdt:P279";p5="wdt:P279";p6="wdt:P279";p7="wdt:P279";p8="wdt:P279";p9="wdt:P279";p10="wdt:P279";p11="wdt:P279";p12="wdt:P279";p13="wdt:P279";p14="wdt:P279";p15="wdt:P279";Message00=T; Message=F
#QID_Name=Label; PropertyPathDepth=n; lang=1; Upper=F

wikiPropertyPath_v2 <- function(QID_Name="wd:Q2374463", PropertyPathDepth=1,
                                lang=1, Upper=F,
                                p1="wdt:P31|wdt:P279",
                                p2="wdt:P31|wdt:P279",
                                p3="wdt:P31|wdt:P279",
                                p4="wdt:P31|wdt:P279",
                                p5="wdt:P31|wdt:P279",
                                p6="wdt:P31|wdt:P279",
                                p7="wdt:P31|wdt:P279",
                                p8="wdt:P31|wdt:P279",
                                p9="wdt:P31|wdt:P279",
                                p10="wdt:P31|wdt:P279",
                                p11="wdt:P31|wdt:P279",
                                p12="wdt:P31|wdt:P279",
                                p13="wdt:P31|wdt:P279",
                                p14="wdt:P31|wdt:P279",
                                p15="wdt:P31|wdt:P279",
                                Message00=F, Message=F){
LABEL <- QID_Name
if(Message00){message(paste(" QID:  ", LABEL, "\n", 
                            "Depth: ", PropertyPathDepth), sep="")}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

#Query <-paste('SELECT distinct ?oj ',  
#              'WHERE { ', LABEL, ' rdfs:label ?oj . filter(LANG(?oj) = "ja")
#              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
#suppressMessages(try(SPA00j <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
#Query <-paste('SELECT distinct ?oe ',
#              'WHERE { ', LABEL, ' rdfs:label ?oe . filter(LANG(?oe) = "en")
#              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
#suppressMessages(try(SPA00e <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
#SPA00 <- c(as.character(SPA00j), as.character(SPA00e))

#try(SPA00[1][identical(SPA00[1], "character(0)")] <- SPA00[2][identical(SPA00[1], "character(0)")])
#try(SPA00[2][identical(SPA00[2], "character(0)")] <- SPA00[1][identical(SPA00[2], "character(0)")])
#try(SPA01 <- as.character(SPA00)[!is.na(as.character(SPA00))])

Query <-paste('SELECT distinct ?oj ?oe ',  
              'WHERE { 
              optional {', LABEL, ' rdfs:label ?oj . filter(LANG(?oj) = "ja") }
              optional {', LABEL, ' rdfs:label ?oe . filter(LANG(?oe) = "en") }
              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
suppressMessages(try(SPA00 <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
SPA01 <- as.character(SPA00)[!is.na(as.character(SPA00))]

if(Upper){
List <- matrix(c(
LABEL,  p1,  '?s1', '.', 
'?s1',  p2,  '?s2', '.',
'?s2',  p3,  '?s3', '.',
'?s3',  p4,  '?s4', '.',
'?s4',  p5,  '?s5', '.',
'?s5',  p6,  '?s6', '.',
'?s6',  p7,  '?s7', '.',
'?s7',  p8,  '?s8', '.',
'?s8',  p9,  '?s9', '.',
'?s9',  p10, '?s10', '.',
'?s10', p11, '?s11', '.',
'?s11', p12, '?s12', '.',
'?s12', p13, '?s13', '.',
'?s13', p14, '?s14', '.',
'?s14', p15, '?s15', '.'), ncol = 4, byrow = T)
COL <- c("subject", "subjectLabel", "subjectLabelAlt", 
         "parentClass", "parentClassLabel", "parentClassLabelAlt",
         "property", "propertyLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label", "?s0LabelAlt",List00[3], paste0(List00[3], "Label ", List00[3], "LabelAlt "), " ?prop ?propLABEL ", collapse = " ")
p00 <- List00[2]
QueryPath00 <- paste0('optional { ', List[PropertyPath00,][1], ' skos:altLabel ?s0LabelAlt . filter(lang(?s0LabelAlt) = "ja")} ',
                      List[PropertyPath00,][1], ' ?p ?s1 . filter(?p IN (wdt:P279, wdt:P31))
                      ?prop wikibase:directClaim ?p . ?prop rdfs:label ?propLABEL . filter(lang(?propLABEL) = "ja") 
                      optional {', List00[3] , ' skos:altLabel ', List00[3],  'LabelAlt. filter(lang(', List00[3], 'LabelAlt) = "ja")}')
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(1)], paste0(List00[PropertyPath00,c(1)], "Label ", List00[PropertyPath00,c(1)], "LabelAlt"),
                        List00[PropertyPath00,c(3)], paste0(List00[PropertyPath00,c(3)], "Label ", List00[PropertyPath00,c(3)], "LabelAlt"),
                        "?prop ?propLABEL ", collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
QueryPath00 <- paste0('optional { ', List[PropertyPath00,][1], ' skos:altLabel ', List[PropertyPath00,][1], 'LabelAlt . filter(lang(', List[PropertyPath00,][1], 'LabelAlt) = "ja")} ',
                      List[PropertyPath00,][1], ' ?p ', List[PropertyPath00,][3], ' . filter(?p IN (wdt:P279, wdt:P31))
                      ?prop wikibase:directClaim ?p . ?prop rdfs:label ?propLABEL . filter(lang(?propLABEL) = "ja") 
                      optional {', List[PropertyPath00,][3] , ' skos:altLabel ', List[PropertyPath00,][3],  'LabelAlt . filter(lang(', List[PropertyPath00,][3], 'LabelAlt) = "ja")}')
}
}else{
List <- matrix(c(
'?s1',  p1,   LABEL , '.', 
'?s2',  p2,  '?s1', '.',
'?s3',  p3,  '?s2', '.',
'?s4',  p4,  '?s3', '.',
'?s5',  p5,  '?s4', '.',
'?s6',  p6,  '?s5', '.',
'?s7',  p7,  '?s6', '.',
'?s8',  p8,  '?s7', '.',
'?s9',  p9,  '?s8', '.',
'?s10', p10, '?s9', '.',
'?s11', p11, '?s10', '.',
'?s12', p12, '?s11', '.',
'?s13', p13, '?s12', '.',
'?s14', p14, '?s13', '.',
'?s15', p15, '?s14', '.'), ncol = 4, byrow = T)

COL <- c("subject", "subjectLabel", "subjectLabelAlt", 
         "childClass", "childClassLabel", "childClassLabelAlt",
         "property", "propertyLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label", "?s0LabelAlt", List00[1], paste0(List00[1], "Label ", List00[1], "LabelAlt "), " ?prop ?propLABEL ", collapse = " ")

p00 <- List00[2]
QueryPath00 <- paste0('optional { ', List[PropertyPath00,][3], ' skos:altLabel ?s0LabelAlt . filter(lang(?s0LabelAlt) = "ja")} ',
                      '?s1 ?p ', List[PropertyPath00,][3], '. filter(?p IN (wdt:P279, wdt:P31))
                      ?prop wikibase:directClaim ?p . ?prop rdfs:label ?propLABEL . filter(lang(?propLABEL) = "ja") 
                      optional {', List00[1] , ' skos:altLabel ', List00[1],  'LabelAlt. filter(lang(', List00[1], 'LabelAlt) = "ja")}')

}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(3)], paste0(List00[PropertyPath00,c(3)], "Label ", List00[PropertyPath00,c(3)], "LabelAlt"),
                        List00[PropertyPath00,c(1)], paste0(List00[PropertyPath00,c(1)], "Label ", List00[PropertyPath00,c(1)], "LabelAlt"),
                        "?prop ?propLABEL ", collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
QueryPath00 <- paste0('optional { ', List[PropertyPath00,][3], ' skos:altLabel ', List[PropertyPath00,][3], 'LabelAlt . filter(lang(', List[PropertyPath00,][3], 'LabelAlt) = "ja")} ',
                      List[PropertyPath00,][1], ' ?p ', List[PropertyPath00,][3], ' . filter(?p IN (wdt:P279, wdt:P31))
                      ?prop wikibase:directClaim ?p . ?prop rdfs:label ?propLABEL . filter(lang(?propLABEL) = "ja") 
                      optional {', List[PropertyPath00,][1] , ' skos:altLabel ', List[PropertyPath00,][1],  'LabelAlt . filter(lang(', List[PropertyPath00,][1], 'LabelAlt) = "ja")}')
}
}

Query <-paste('SELECT distinct ', PropertyPath01, 
              ' WHERE {',  QueryPath, QueryPath00, 
              ' SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', 
              sep=" ")
  
if(Message){message(paste("Query: ", LABEL, sep=""))}else{message(paste("", sep=""))}
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(nrow(SPA) == 0){return(NULL)}

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

if(any(colnames(SPA) == "s0")){
  SPA$s0 <- LABEL
  SPA$s0Label <- SPA01[1]
}
#head(SPA)
if(Message00){message(paste(" Results" , "\n Data number:  ", nrow(SPA), sep=""))}
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,1] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,1]), silent = T)
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,3] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,3]), silent = T)
try(SPA[,grep(pattern="prop$", colnames(SPA))] <- gsub("http://www.wikidata.org/entity/", "wdt:", 
  SPA[,grep(pattern="prop$", colnames(SPA))]), silent = T)

colnames(SPA) <- COL
return(data.frame(QID=LABEL, LabeL=SPA01[1], SPA, p=p00))

}

#############################################################################
#############################################################################
#############################################################################
wikiPropertyPath_v1 <- function(QID_Name="wd:Q2374463", PropertyPathDepth=1,
                                lang=1, Upper=F,
                                p1="wdt:P31|wdt:P279",
                                p2="wdt:P279",
                                p3="wdt:P279",
                                p4="wdt:P279",
                                p5="wdt:P279",
                                p6="wdt:P279",
                                p7="wdt:P279",
                                p8="wdt:P279",
                                p9="wdt:P279",
                                p10="wdt:P279",
                                p11="wdt:P279",
                                p12="wdt:P279",
                                p13="wdt:P279",
                                p14="wdt:P279",
                                p15="wdt:P279",
                                Message00=F, Message=F){
LABEL <- QID_Name
if(Message00){message(paste(" QID:  ", LABEL, "\n", 
                            "Depth: ", PropertyPathDepth), sep="")}
if(lang == 1){ lang1 <- "ja, en" } else { if(lang == 2){ lang1 <- "ja" } else { lang1 <- "en" } }

Prefix <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

#Query <-paste('SELECT distinct ?oj ',  
#              'WHERE { ', LABEL, ' rdfs:label ?oj . filter(LANG(?oj) = "ja")
#              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
#suppressMessages(try(SPA00j <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
#Query <-paste('SELECT distinct ?oe ',
#              'WHERE { ', LABEL, ' rdfs:label ?oe . filter(LANG(?oe) = "en")
#              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
#suppressMessages(try(SPA00e <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
#SPA00 <- c(as.character(SPA00j), as.character(SPA00e))

#try(SPA00[1][identical(SPA00[1], "character(0)")] <- SPA00[2][identical(SPA00[1], "character(0)")])
#try(SPA00[2][identical(SPA00[2], "character(0)")] <- SPA00[1][identical(SPA00[2], "character(0)")])
#try(SPA01 <- as.character(SPA00)[!is.na(as.character(SPA00))])

Query <-paste('SELECT distinct ?oj ?oe ',  
              'WHERE { 
              optional {', LABEL, ' rdfs:label ?oj . filter(LANG(?oj) = "ja") }
              optional {', LABEL, ' rdfs:label ?oe . filter(LANG(?oe) = "en") }
              SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', sep="")
suppressMessages(try(SPA00 <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))
SPA01 <- as.character(SPA00)[!is.na(as.character(SPA00))]

if(Upper){
List <- matrix(c(
LABEL,  p1,  '?s1', '.', 
'?s1',  p2,  '?s2', '.',
'?s2',  p3,  '?s3', '.',
'?s3',  p4,  '?s4', '.',
'?s4',  p5,  '?s5', '.',
'?s5',  p6,  '?s6', '.',
'?s6',  p7,  '?s7', '.',
'?s7',  p8,  '?s8', '.',
'?s8',  p9,  '?s9', '.',
'?s9',  p10, '?s10', '.',
'?s10', p11, '?s11', '.',
'?s11', p12, '?s12', '.',
'?s12', p13, '?s13', '.',
'?s13', p14, '?s14', '.',
'?s14', p15, '?s15', '.'), ncol = 4, byrow = T)
COL <- c("subject", "subjectLabel", 
         "parentClass", "parentClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[3], paste(List00[3], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}
}else{
List <- matrix(c(
'?s1',  p1,   LABEL , '.', 
'?s2',  p2,  '?s1', '.',
'?s3',  p3,  '?s2', '.',
'?s4',  p4,  '?s3', '.',
'?s5',  p5,  '?s4', '.',
'?s6',  p6,  '?s5', '.',
'?s7',  p7,  '?s6', '.',
'?s8',  p8,  '?s7', '.',
'?s9',  p9,  '?s8', '.',
'?s10', p10, '?s9', '.',
'?s11', p11, '?s10', '.',
'?s12', p12, '?s11', '.',
'?s13', p13, '?s12', '.',
'?s14', p14, '?s13', '.',
'?s15', p15, '?s14', '.'), ncol = 4, byrow = T)
COL <- c("subject", "subjectLabel", 
         "childClass", "childClassLabel")

PropertyPath00 <- PropertyPathDepth
List00 <- List[c(1:PropertyPath00),]

if(PropertyPath00 == 1){
QueryPath <- paste(List00, collapse = " ")
PropertyPath01 <- paste("?s0", "?s0Label",
                        List00[1], paste(List00[1], "Label" ,sep=""), collapse = " ")
p00 <- List00[2]
}else{
QueryPath <- paste(t(List00), collapse = " ")
PropertyPath01 <- paste(List00[PropertyPath00,c(3)], paste(List00[PropertyPath00,c(3)], "Label" ,sep=""),
                        List00[PropertyPath00,c(1)], paste(List00[PropertyPath00,c(1)], "Label" ,sep=""),
                        collapse = " ")
p00 <- List00[PropertyPath00,c(2)]
}}

Query <-paste('SELECT distinct ', PropertyPath01, 
              ' WHERE {',  QueryPath, ' SERVICE wikibase:label { bd:serviceParam wikibase:language "', lang1, '".}}', 
              sep="")
  
if(Message){message(paste("Query: ", LABEL, sep=""))}else{message(paste("", sep=""))}
suppressMessages(try(SPA <- data.frame(query_wikidata(paste(Prefix, Query), format = "simple")), silent = T))

if(nrow(SPA) == 0){return(NULL)}

if(exists("SPA")){}else{
  return(message("Perhaps No Internet Services"))}

if(any(colnames(SPA) == "s0")){
  SPA$s0 <- LABEL
  SPA$s0Label <- SPA01[1]
}

if(Message00){message(paste(" Results" , "\n Data number:  ", nrow(SPA), sep=""))}
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,1] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,1]), silent = T)
try(SPA[,-grep(pattern="Label$", colnames(SPA))][,2] <- gsub("http://www.wikidata.org/entity/", "wd:", 
  SPA[,-grep(pattern="Label$", colnames(SPA))][,2]), silent = T)

colnames(SPA) <- COL
return(data.frame(QID=LABEL, LabeL=SPA01[1], SPA, p=p00))

}

#Test
#a <- wikiPropertyPath_v1(QID_Name="wd:Q178593", PropertyPathDepth=1)
#a <- wikiPropertyPath_v1(QID_Name="wd:Q178593", PropertyPathDepth=2, Message00=T)
#a <- wikiPropertyPath_v1(QID_Name="wd:Q178593", PropertyPathDepth=3)
#wikiPropertyPath_v1(QID_Name="wd:Q178593", PropertyPathDepth=4)
#head(a)
