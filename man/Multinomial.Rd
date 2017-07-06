\name{Multinomial}
\alias{Multinomial}
\title{Generation of Multinomial Random Samples}
\description{It generates a data matrix with random samples from a multinomial distribution where the rows are the samples and the columns are the taxa.}

\usage{Multinomial(Nrs, probs)}

\arguments{
  \item{Nrs}{A vector specifying the number of reads or sequence depth for each sample.}
  \item{probs}{A vector specifying taxa probabilities.}
}

\value{A data matrix of taxa counts where the rows are the samples and the columns are the taxa.}

\details{
	Note: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	### Generate the number of reads per sample
	### The first number is the number of reads and the second is the number of subjects
	nrs <- rep(15000, 25)
	
	### Create a probability vector
	probs <- c(0.4, 0.3, 0.2, .05, 0.04, .01)
	
	mData <- Multinomial(nrs, probs)
	mData[1:5, 1:5]
}