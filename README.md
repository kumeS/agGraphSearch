# agGraphSearch (ver 0.99.x)

## Introduction

agGraphSearch package supplies a tool-set for searching graph structures based on RDF (Resource Description Framework)
and extracting the subset of the class-related hierarchy with domain specific terms.

This set of functions also allows us to explore the triple-like formatted dataset 
without creating any SPARQL queries on R scripts.

See [the workflow submitted to IJCKG2021](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-Wikidata-WF.html)

## Installation

1. Start R.app

2. Run the following commands in the R console.

```r
#install SPARQL_1.16
URL <- "https://cran.r-project.org/src/contrib/Archive/SPARQL/SPARQL_1.16.tar.gz"
install.packages(URL, repos=NULL, type="source")

#install agGraphSearch
install.packages( "devtools" )
devtools::install_github( "kumeS/agGraphSearch" )
library( "agGraphSearch" )
```

## Tutorial/workflow

- [Installation](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-installation.html)

- [Installation (Japanese)](https://kumes-github-io.translate.goog/agGraphSearch/vignettes/agGraphSearch-installation.html?_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=nui)

- [A short-tutorial for agGraphSearch](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-short-tutorial.html)

- [A Japanese short-tutorial for agGraphSearch](https://translate.google.com/translate?sl=en&tl=ja&u=https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-short-tutorial.html)

- [[Wikidata] agGraphSearch tutorial: A workflow to use agGraphSearch and Wikidata with PolyInfo terms](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-Wikidata-WF.html)
  - Data Dump: [Google Drive](https://drive.google.com/drive/folders/1nk9nubKnX1uzcBf3GmdaijdGeqDoHucz?usp=sharing)
  - [Pre-processing of Dump Data](https://github.com/kumeS/AHLodDbs)
  
- [[Mesh] agGraphSearch tutorial: A workflow to use agGraphSearch and Mesh RDF with PolyInfo terms](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-Mesh-WF-PolyInfo.html)
  - Data Dump: [Google Drive](https://drive.google.com/drive/folders/1qMSSfQ5maJsXj0FcDGhc5gnThMobpSSY?usp=sharing)
  - [Pre-processing of Dump Data](https://github.com/kumeS/AHLodDbs)

- [[ChEBI] A workflow to use agGraphSearch and ChEBI RDF](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-ChEBI-WF-PolymerDic.html)
  - Data Dump: [Google Drive](https://drive.google.com/drive/folders/1KF54X4BH8RPZP1v0rnZWIHeBCsY083iI?usp=sharing)
  - [Pre-processing of Dump Data](https://github.com/kumeS/AHLodDbs)
  
  
- [The workflow submitted to IJCKG2021 (The 10th International Joint Conference on Knowledge Graphs)](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-Wikidata-WF.html)

## Some examples of function execution in the package

- CkeckQuery_agCount_Label_Num_Wikidata_P279_P31

<img src="inst/images/CkeckQuery_agCount_Label_Num_Wikidata_P279_P31.png" width="400px">

- agCount_Label_Num_Wikidata_P279_P31 & agTableDT

<img src="inst/images/agCount_Label_Num_Wikidata_P279_P31.png" width="400px">

- CkeckQuery_agWD_Alt_Wikidata

<img src="inst/images/CkeckQuery_agWD_Alt_Wikidata.png" width="400px">

- CkeckQuery_agQIDtoLabel_Wikidata

<img src="inst/images/CkeckQuery_agQIDtoLabel_Wikidata.png" width="400px">

- CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31

<img src="inst/images/CkeckQuery_agCount_ID_Num_Wikidata_QID_P279_P31.png" width="400px">

## SPARQL endpoints 

- KzLab at OECU
  - Wikidata
    - Endpoint URL: http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj
    - Graph ID: http://wikidata_nearly_full_201127

  - Polymer ontology 1.1
    - Endpoint URL: http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj_11

  - DBpedia Japanese 2016
    - Endpoint URL: http://kozaki-lab.osakac.ac.jp/agraph/DBpedia2016_RDF
    - Graph ID: http://DBpedia_2016_jpn

  - IOBC / Mesh / LSD / Nikkaji / ICD10
    - Endpoint URL: http://kozaki-lab.osakac.ac.jp/agraph/IOBC_Mesh_LOD
		- Graph ID:
		  - http://iobc
		  - http://mesh
		  - http://lsd
		  - http://nikkaji
		  - http://icd10

## Optional setting at OECU

Run the following commands in the R console.

```r
#Proxy setting at OECU
proxy_url = "http://wwwproxy.osakac.ac.jp:8080"
Sys.setenv("http_proxy" = proxy_url)
Sys.setenv("https_proxy" = proxy_url)
Sys.setenv("ftp_proxy" = proxy_url)

#Test 1
curlGetHeaders("http://www.google.com/")
# OK if the optput is 200
install.packages("readr")
# OK if it is installed successfully.

#Test 2
install.packages("BiocManager")
BiocManager::install("BiocStyle")
install.packages( "devtools" )
devtools::install_github( "kumeS/agGraphSearch" )
library( "agGraphSearch" )

#Test 3
#system( "git clone https://github.com/kumeS/agGraphSearch.git" )
```

## Author / maintainer

- Satoshi Kume

## License

[Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0).

## Citation

```
@inproceedings{10.1145/3502223.3502227,
author = {Kume, Satoshi and Kozaki, Kouji},
title = {Extracting Domain-Specific Concepts from Large-Scale Linked Open Data},
year = {2022},
isbn = {9781450395656},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
url = {https://doi.org/10.1145/3502223.3502227},
doi = {10.1145/3502223.3502227},
abstract = {We propose a methodology for extracting concepts for a target domain from large-scale linked open data (LOD) to support the construction of domain ontologies providing field-specific knowledge and definitions. The proposed method defines search entities by linking the LOD vocabulary with technical terms related to the target domain. The search entities are then used as a starting point for obtaining upper-level concepts in the LOD, and the occurrences of common upper-level entities and the chain-of-path relationships are examined to determine the range of conceptual connections in the target domain. A technical dictionary index and natural language processing are used to evaluate whether the extracted concepts cover the domain. As an example of extracting a class hierarchy from LOD, we used Wikidata to construct a domain ontology for polymer materials and physical properties. The proposed method can be applied to general datasets with class hierarchies, and it allows ontology developers to create an initial model of the domain ontology for their own purposes.},
booktitle = {The 10th International Joint Conference on Knowledge Graphs},
pages = {28–37},
numpages = {10},
keywords = {Linked open data, Ontology construction, Domain ontology, Wikidata, Graph analysis},
location = {Virtual Event, Thailand},
series = {IJCKG'21}
}
```
