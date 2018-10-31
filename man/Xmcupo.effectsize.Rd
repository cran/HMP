\name{Xmcupo.effectsize}
\alias{Xmcupo.effectsize}
\title{Effect Size for Xmcupo Statistic}
\description{This function computes the Cramer's Phi and Modified Cramer's Phi Criterion for the test statistic \code{Xmcupo.sevsample}.}

\usage{Xmcupo.effectsize(group.data)}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
}

\details{Note: The matrices in \code{group.data} must contain the same taxa, in the same order.}

\value{A vector containing the Chi-Squared statistic value, the Cramer's Phi Criterion, and the modified Cramer's Phi Criterion.}

\examples{
	data(saliva)
	data(throat)

	### Combine the data sets into a single list
	group.data <- list(saliva, throat)
	
	effect <- Xmcupo.effectsize(group.data)
	effect
}