% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agTableFM.R
\name{agTableFM}
\alias{agTableFM}
\title{Interactive html table using formattable package.}
\usage{
agTableFM(
  Data,
  Output = TRUE,
  Browse = TRUE,
  File = paste0("agTableFM_table_", format(Sys.time(), "\%y\%m\%d_\%H\%M"), ".html")
)
}
\arguments{
\item{Data}{an input as a data.frame.}

\item{Output}{logical; if TRUE, create the output file as a name of File.}

\item{Browse}{logical.}

\item{File}{a string of file name.}
}
\value{
formattable::formattable
}
\description{
this function creates a interactive table for checking the data.
}
\examples{
\dontrun{

print(iris)

#run
agTableFM( Data = iris )

}
}
\author{
Satoshi Kume
}
