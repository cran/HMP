\name{Barchart.data}
\alias{Barchart.data}
\title{A Graphical Representation of Taxa Proportions}
\description{Creates a bar plot of taxonomic proportions.}
\usage{Barchart.data(data, level)}

\arguments{
  \item{data}{
A data matrix of taxonomic counts(columns) for each sample(rows), at the specified taxonomic level.}
  \item{level}{
A string indicating taxonomic level.}
}

\value{A bar plot of taxonomic proportions for all samples at a given taxonomic level.}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}
\examples{
data(saliva)
Barchart.data(saliva, 'genus')
}