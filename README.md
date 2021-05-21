# agGraphSearch (under development)

## Intoduction

agGraphSearch, a R package, supply a tool-set for searching graph data 
structures of RDF (Resource Description Framework) via SPARQL query.

This set of functions allows us to explore the graph data (triples) 
as a function of R, without having to create any SPARQL queries on R scripts.

## Installation

1. Start R.app

2. Run the following commands in the R console.

```r
install.packages( "devtools" )
devtools::install_github( "kumeS/agGraphSearch" )
library( "agGraphSearch" )
```

An alternative way,
if you use MacOSX, install Git by installing Homebrew on Terminal first.

```r
system("brew install git")
```

Then,

```r
system( "git clone https://github.com/kumeS/agGraphSearch.git" )
system( "R CMD INSTALL agGraphSearch" )
library( "agGraphSearch" )
```

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

## Tutorial

- [Installation & Basic functions](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-installation.html)

- [Installation & Basic functions 日本語チュートリアル](http://translate.google.com/translate?hl=&sl=en&tl=ja&u=https%3A%2F%2Fkumes.github.io%2FagGraphSearch%2Fvignettes%2FagGraphSearch-installation.html)

- [A short-tutorial for agGraphSearch](https://kumes.github.io/agGraphSearch/vignettes/agGraphSearch-short-tutorial.html)

- [agGraphSearch 日本語ショートチュートリアル](http://translate.google.com/translate?hl=&sl=en&tl=ja&u=https%3A%2F%2Fkumes.github.io%2FagGraphSearch%2Fvignettes%2FagGraphSearch-short-tutorial.html)

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

## Author / maintainer

- Satoshi Kume

## License

[Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0).

