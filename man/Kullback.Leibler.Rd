\name{Kullback.Leibler}
\alias{Kullback.Leibler}
\alias{kullbackLeiber}
\title{Kullback Leibler}
\description{Calculates Kullback Leibler divergence for all pairs of the datasets.}

\usage{
	Kullback.Leibler(group.data, plot = TRUE, main="Kullback Leibler Divergences", 
		parallel = FALSE, cores = 3)
}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
  \item{plot}{When 'TRUE' a heatmap of the results will also be generated.}
  \item{main}{A string to be used as the plots title.}
  \item{parallel}{When this is 'TRUE' it allows for parallel calculation of the KL distances. Requires the package \code{doParallel}.}
  \item{cores}{The number of parallel processes to run if parallel is 'TRUE'.}
}

\value{A matrix of Kullback Leibler divergence values and a heatmap if plot is TRUE.}

\references{
	Kotz S, Johnson N.L (1981) Encyclopedia Of Statistical Sciences
}

\examples{
	data(saliva)
	data(throat)
	data(tonsils)
	
	### Combine the data sets into a single list
	group.data <- list(saliva, throat, tonsils)
	
	\dontrun{
		kl <- Kullback.Leibler(group.data)
		kl
	}
}
