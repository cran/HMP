\name{Data.filter}
\alias{Data.filter}
\title{A Data Filter}
\description{
This function creates a new dataset from an existing one that collapses less-abundant taxa into one category as 
specified by the user and excludes samples with a total number of reads fewer than the user-specified value.}
\usage{Data.filter(data, order.type, reads.crit, K)}

\arguments{
  \item{data}{
A data matrix of taxonomic counts(columns) for each sample(rows).}
  \item{order.type}{
	If 'sample': Rank taxa based on its taxonomic frequency. \cr
	If 'data': Rank taxa based on cumulative taxonomic counts across all samples.}
  \item{reads.crit}{Samples with a total number of reads less than read.crit's value will be deleted.}
  \item{K}{The K most abundant taxa.}
}

\value{
Data frame with K+1 ranked columns and the number of rows equal to number of samples with a total number of reads greater than the critical value. 
The (K+1)th taxon contains the sum of the remaining less abundant taxa equal to (number of columns-K).}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 

### Excludes all samples with fewer than 1000 reads and collapses
### taxas with 11th or smaller abundance into one category 
myfiltered <- Data.filter(saliva, "sample", 1000, 10) 
}