\name{kullbackLeiber}
\alias{kullbackLeiber}
\title{Kullback Leiber}
\description{
	This function calculates Kullback Leiber divergence for all pairwise combinations of the datasets given.
}

\usage{kullbackLeiber(data, plot = TRUE, parallel = FALSE, cores = 3)}

\arguments{
  \item{data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
  \item{plot}{When 'TRUE' a heatmap of the results will also be generated.}
  \item{parallel}{When this is 'TRUE' it allows for parallel calculation of the bootstraps. (See Note 1 in Details).}
  \item{cores}{The number of parallel processes to run if parallel is 'TRUE'.}
}

\details{
	Notes: 
	\enumerate{
		\item Enabling parallel calculation requires the package \code{doParallel}.
	}
}

\value{A matrix of Kullback Leibler divergences and a heatmap if plot is TRUE.}

\author{Sharina Carter, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	data(tonsils)
	
	mygroup <- list(sa=saliva, th=throat, to=tonsils)
	
	\dontrun{
		kl <- kullbackLeiber(mygroup)
		kl
	}
}
