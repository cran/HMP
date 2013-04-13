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

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- dirmult(saliva) 
	
	### Change the number of display digits so our output looks better
	defaultd <- .Options$digits
	options(digits=5)
	
	xsc.one_check <- Xsc.onesample(throat, fit.saliva$pi)
	xsc.one_check
	
	options(digits=defaultd) ### Set the number of digits back for the user
}