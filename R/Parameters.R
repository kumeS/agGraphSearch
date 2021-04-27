##' @title Default Parameters
##' @export KzLabEndPoint_Wikidata
##' @export wikidataClassProperty
##' @export PREFIX
##' @export URI2Prefix

#########################################################################
#Default Parameters
#these parameters are for developer, and are used as "agGraphSearch:::".
#########################################################################
#wikidata
KzLabEndPoint_Wikidata <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj",
                               FROM = "From <http://wikidata_nearly_full_201127> ")
wikidataClassProperty <- list(p1="wdt:P279", p2="wdt:P31")


#IOBC
KzLabEndPoint_IOBC <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                           FROM = "From <http://iobc> ")
iobcClassProperty <- list(p1="XXX", p2="XXX")

#Mesh
KzLabEndPoint_MESH <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                           FROM = "From <http://mesh> ")
meshClassProperty <- list(p1="XXX", p2="XXX")

#LSD
KzLabEndPoint_LSD <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                          FROM = "From <http://lsd> ")
lsdClassProperty <- list(p1="XXX", p2="XXX")

#nikkaji
KzLabEndPoint_nikkaji <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                              FROM = "From <http://nikkaji> ")
nikkajiClassProperty <- list(p1="XXX", p2="XXX")

#Prefix
PREFIX <- '
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX wikibase: <http://wikiba.se/ontology#>
'

URI2Prefix0 <- t(data.frame(wd=c("wd:", "<http://www.wikidata.org/entity/", ">"),
                         wdt=c("wdt:", "<http://www.wikidata.org/prop/direct/", ">"),
                         rdfs=c("rdfs:", "<http://www.w3.org/2000/01/rdf-schema#", ">"),
                         skos=c("skos:", "<http://www.w3.org/2004/02/skos/core#", ">"),
                         rdf=c("rdf:", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#", ">"),
                         owl=c("owl:", "<http://www.w3.org/2002/07/owl#", ">"),
                         dct=c("dct:", "<http://purl.org/dc/terms/", ">"),
                         foaf=c("foaf:", "<http://xmlns.com/foaf/0.1/", ">"),
                         wikibase=c("wikibase:", "<http://wikiba.se/ontology#", ">")))
rownames(URI2Prefix0) <- 1:nrow(URI2Prefix0)
colnames(URI2Prefix0) <- c("prefix", "uri01", "uri02")
URI2Prefix <- URI2Prefix0



