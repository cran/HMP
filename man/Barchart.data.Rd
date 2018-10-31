\name{Barchart.data}
\alias{Barchart.data}
\title{A Graphical Representation of Taxa Proportions}
\description{Creates a bar plot of taxonomic proportions.}

\usage{Barchart.data(data, title = "Taxa Proportions")}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
  \item{title}{A string to be used as the plots title. The default is "Taxa Proportions".}
}

\value{A bar plot of taxonomic proportions for all samples at a given taxonomic level.}

\examples{
	data(saliva)
	
	Barchart.data(saliva)
}