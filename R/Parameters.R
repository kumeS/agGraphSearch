##' @title Default Parameters
##' @export KzLabEndPoint_Wikidata
##' @export wikidataClassProperty
##'
##' @export PREFIX
##' @export URI2Prefix
##' @export PREFIX_Mesh
##'
##' @export LabelProperty01
##' @export LabelProperty02
##'
##' @export meshClassProperty
##' @export KzLabEndPoint_MESH
##' @export EndPoint_MeshRDF
##' @export MeshRDF_ClassProperty
##'
##'

#######################################################################################################
#Default Parameters
#these parameters are for developer, and are used as "agGraphSearch::" / "agGraphSearch:::" .
#######################################################################################################
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
meshClassProperty <- list(p1="meshv:broaderDescriptor", p2="")

#LSD
KzLabEndPoint_LSD <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                          FROM = "From <http://lsd> ")
lsdClassProperty <- list(p1="XXX", p2="XXX")

#nikkaji
KzLabEndPoint_nikkaji <- list(EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD",
                              FROM = "From <http://nikkaji> ")
nikkajiClassProperty <- list(p1="XXX", p2="XXX")

#Mesh RDF original
EndPoint_MeshRDF <- list(EndPoint="https://id.nlm.nih.gov/mesh/query",
                         FROM = "From <http://id.nlm.nih.gov/mesh> ")
MeshRDF_ClassProperty <- list(p1="meshv:broaderDescriptor", p2="")


#Label property
LabelProperty01 <- list(prop1="rdfs:label",
                        prop2="skos:altLabel")
LabelProperty02 <- list(prop1="rdfs:label",
                        prop2="meshv:altLabel")

#######################################################################################################
#Prefix
#######################################################################################################
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

PREFIX_Mesh <- '
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX meshv: <http://id.nlm.nih.gov/mesh/vocab#>
PREFIX mesh: <http://id.nlm.nih.gov/mesh/>
PREFIX mesh2021: <http://id.nlm.nih.gov/mesh/2021/>
PREFIX mesh2020: <http://id.nlm.nih.gov/mesh/2020/>
PREFIX mesh2019: <http://id.nlm.nih.gov/mesh/2019/>
'

#######################################################################################################
#URI2Prefix
#######################################################################################################
URI2Prefix0 <- data.frame(t(data.frame(wd=c("wd:", "<http://www.wikidata.org/entity/", ">"),
                         wdt=c("wdt:", "<http://www.wikidata.org/prop/direct/", ">"),
                         rdfs=c("rdfs:", "<http://www.w3.org/2000/01/rdf-schema#", ">"),
                         skos=c("skos:", "<http://www.w3.org/2004/02/skos/core#", ">"),
                         rdf=c("rdf:", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#", ">"),
                         owl=c("owl:", "<http://www.w3.org/2002/07/owl#", ">"),
                         dct=c("dct:", "<http://purl.org/dc/terms/", ">"),
                         foaf=c("foaf:", "<http://xmlns.com/foaf/0.1/", ">"),
                         wikibase=c("wikibase:", "<http://wikiba.se/ontology#", ">"),
                         xsd=c("xsd:", "<http://www.w3.org/2001/XMLSchema#", ">"),
                         meshv=c("meshv:", "<http://id.nlm.nih.gov/mesh/vocab#", ">"),
                         mesh=c("mesh:", "<http://id.nlm.nih.gov/mesh/", ">"),
                         mesh2021=c("mesh2021:", "<http://id.nlm.nih.gov/mesh/2021/", ">"),
                         mesh2020=c("mesh2020:", "<http://id.nlm.nih.gov/mesh/2020/", ">"),
                         mesh2019=c("mesh2019:", "<http://id.nlm.nih.gov/mesh/2019/", ">"))))

rownames(URI2Prefix0) <- 1:nrow(URI2Prefix0)
colnames(URI2Prefix0) <- c("prefix", "uri01", "uri02")
URI2Prefix <- URI2Prefix0

#######################################################################################################
#######################################################################################################







