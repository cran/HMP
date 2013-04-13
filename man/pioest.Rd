\name{pioest}
\alias{pioest}
\title{Weighted Average of Taxa Frequency for Several Groups}
\description{This function computes a weighted-average of taxa frequency estimated from several groups.}

\usage{pioest(group.data)}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
}
\details{Note: The matrices in \code{group.data} must contain the same taxa, in the same order.}

\value{A vector containing the weighted-average of taxa frequency.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	group.data <- list(saliva, throat)
	
	pio <- pioest(group.data)
	pio
}