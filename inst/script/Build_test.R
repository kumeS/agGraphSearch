rm(list=ls())
getwd()
dir()

#system("R CMD build --keep-empty-dirs --no-resave-data agGraphSearch")
#system("R CMD check --no-vignettes --timings --no-multiarch agGraphSearch_0.99.1.tar.gz")

##up-to-date
roxygen2::roxygenise("./agGraphSearch")
system("./agGraphSearch_github_oecu.command")

##Local
roxygen2::roxygenise("./agGraphSearch")
system("R CMD INSTALL agGraphSearch")
library(agGraphSearch)

#Search from ID
ID <- "wd:Q81163"
CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31(Entity_ID = ID)
agCount_ID_Num_Wikidata_QID_P279_P31( Entity_ID = ID, Message=TRUE)

#Search from Label
Label <- "polymer"
CkeckQuery_agCount_Label_Num_Wikidata_P279_P31(Entity_Name = Label)
agCount_Label_Num_Wikidata_P279_P31(Entity_Name = Label)

Label <- "ポリマー"
CkeckQuery_agCount_Label_Num_Wikidata_P279_P31(Entity_Name = Label)
agCount_Label_Num_Wikidata_P279_P31(Entity_Name = Label)



##From GitHub
require("devtools")
devtools::install_github("kumeS/agGraphSearch")
library("agGraphSearch")

##From GitHub
system("git clone https://github.com/kumeS/agGraphSearch.git ")
system("R CMD INSTALL agGraphSearch")
library(agGraphSearch)

