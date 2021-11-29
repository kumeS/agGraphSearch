#Convert the extracted results to N-triples
convRDF_ChEBI <- function(Dat02=Dat02,
                         LabList00=LabList00,
                         FileName="./05_ChEBI_OCR_05_RDF/RDF_from_ChEBI_v01.nt"){

#class hierarchy
b <- Dat02
head(b)

#hist(nchar(unique(c(b$subjectLabel, b$parentClassLabel))))
#unique(c(b$subjectLabel, b$parentClassLabel))[nchar(unique(c(b$subjectLabel, b$parentClassLabel))) < 100]

#Check NA
#table(is.na(b))

#Remove the loops
head(b)
table(b$subject != b$parentClass)
b <- b[b$subject != b$parentClass, ]

#table(grepl("obo[:]", b$subject))
#table(grepl("obo[:]", b$parentClass))

nt <- paste0("<", sub("obo[:]", "http://purl.obolibrary.org/obo/", b$subject), "> ",
            "<http://www.w3.org/2000/01/rdf-schema#subClassOf> ",
            "<", sub("obo[:]", "http://purl.obolibrary.org/obo/", b$parentClass), "> .")
nt <- unique(nt)

write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=F, quote = F)
colnames(b)

#Labels: b
nt <- paste0('<', sub("obo[:]", "http://purl.obolibrary.org/obo/", b$subject), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', b$subjectLabel, '"@en.')
nt <- unique(nt)
write.table(nt, FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

nt <- paste0('<', sub("obo[:]", "http://purl.obolibrary.org/obo/", b$parentClass), '> ',
            '<http://www.w3.org/2000/01/rdf-schema#label> ',
            '"', b$parentClassLabel, '"@en.')
nt <- unique(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

#Search entity
cc <- LabList00
nt <- paste0('<', sub("obo[:]", "http://purl.obolibrary.org/obo/", cc), '> ',
            '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://class/search> .')
nt <- unique(nt)
write.table(nt, file=FileName, sep="",
             row.names = F, col.names = F, append=T, quote = F)

##################################################################
#Aggregation and De-duplication
e <- readLines(FileName)
print(str(e))
print(length(unique(e)))
##################################################################
#Save
write.table(data.frame(X=unique(e)),
            file=FileName, sep="",
            row.names = F, col.names = F, append=F, quote = F)
##################################################################

}




