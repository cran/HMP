\name{Multinomial}
\alias{Multinomial}
\title{Generation of Multinomial Random Samples}
\description{
It generates a data matrix with random samples from a multinomial distribution where the rows are the samples and the columns are the taxa.
}
\usage{Multinomial(Nrs, probs)}

\arguments{
  \item{Nrs}{
A vector specifying the number of reads or sequence depth for each sample.}
  \item{probs}{
A vector specifying taxa probabilities.}
}

\value{A data matrix of taxa counts where the rows are the samples and the columns are the taxa.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
### Generate a random vector of number of reads per sample
q <- seq(5000, 20000, by=50)
Nrs <- sample(q, size=25)
 
mypi <- c(0.4, 0.3, 0.2, .05, 0.04, .01)

mult_data1 <- Multinomial(Nrs, mypi)
mult_data1
}