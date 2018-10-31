\name{Xsc.onesample}
\alias{Xsc.onesample}
\title{Generalized Wald-Type Statistics: One Sample RAD Probability-Mean Test Comparison}
\description{
	This routine provides the value of the Generalized Wald-type statistic to assess whether the RAD probability-mean observed in 
	one group of samples is equal to a known RAD probability-mean.
}

\usage{Xsc.onesample(data, pi0)}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
  \item{pi0}{The RAD-probability mean vector.}
}

\value{A list containing Generalized Wald-type statistics and p-value.}

\examples{
	data(saliva)
	data(throat)
	
	### Get pi from the dirichlet-multinomial parameters
	pi0 <- dirmult(saliva)$pi
	
	xsc <- Xsc.onesample(throat, pi0)
	xsc
}