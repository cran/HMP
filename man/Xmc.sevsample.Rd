\name{Xmc.sevsample}
\alias{Xmc.sevsample}
\title{Generalized Wald-type Statistics: Several Sample RAD Probability-Mean Test Comparison with a Known Common Vector}
\description{
	This function computes the Generalized Wald-type test statistic (Wilson and Koehler, 1984) and corresponding p-value to assess whether the sample 
	RAD probability-means from multiple populations are the same or different. The statistics assumes that a common RAD probability-mean vector for comparison under the null 
	hypothesis is known.
}

\usage{Xmc.sevsample(group.data, pi0)}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
  \item{pi0}{The RAD-probability mean vector.}
}

\details{Note: The matrices in \code{group.data} must contain the same taxa, in the same order.}

\value{A list containing the Generalized Wald-type statistics and p-value.}

\references{
	Wilson, J. R., and Koehler, K. J. (1984). Testing of equality of vectors of proportions for several cluster samples. 
	Proceedings of Joint Statistical Association Meetings. Survey Research Methods.
}

\examples{
	data(saliva) 
	data(throat)
	data(tonsils)
	
	### Get pi from the dirichlet-multinomial parameters
	pi0 <- dirmult(saliva)$pi
	
	### Combine the data sets into a single list
	group.data <- list(throat, tonsils)
	
	xmc <- Xmc.sevsample(group.data, pi0)
	xmc
}