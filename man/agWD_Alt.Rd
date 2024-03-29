% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agWD_Alt.R
\name{agWD_Alt}
\alias{agWD_Alt}
\title{Searching the label corresponding to the entity url via SPARQL.}
\usage{
agWD_Alt(
  Entity_Name,
  EndPoint,
  FROM,
  AltLabel = FALSE,
  Property = 1,
  lang = 1,
  Message = TRUE
)
}
\arguments{
\item{Entity_Name}{a character vector corresponing to the entity label.}

\item{EndPoint}{a string of SPARQL endpoint. ex. http://....}

\item{FROM}{a string of graph URI in the endpoint. The default is blank ("").}

\item{AltLabel}{logical; default is FALSE.}

\item{Property}{a numeric vector.
if the number is 1 (default), the properties are set to be "rdfs:label" and "skos:altLabel".
Still no other options in a parameter of Property.}

\item{lang}{a numeric vector.
if the number is 1 (default), the label contains English label (@en).
if the number is 2, the label contains Japanese label (@ja).
if the number is 3, the labels contain both English and Japanese labels.}

\item{Message}{logical; perform an output of Entity_Name or not.}
}
\value{
data.frame
}
\description{
this function is a general function for
searching the RDF data using an entity URI via SPARQL.
}
\examples{
\dontrun{
#parameters

#polymer (wikidata prefix URI: wd:Q81163)
Label <- "CAS Registry Number"

print(KzLabEndPoint_Wikidata)
print(wikidataClassProperty)

#run
agWD_Alt(
  Entity_Name=Label,
  EndPoint=KzLabEndPoint_Wikidata$EndPoint,
  FROM=KzLabEndPoint_Wikidata$FROM
  )

}
}
\seealso{
list2DF {base}
}
\author{
Satoshi Kume
}
