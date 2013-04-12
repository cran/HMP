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
	### Generate a random vector of number of reads per sample
	Nrs <- rep(15000, 25)
	 
	mypi <- c(0.4, 0.3, 0.2, .05, 0.04, .01)
	
	mult_data <- Multinomial(Nrs, mypi)
	mult_data
}