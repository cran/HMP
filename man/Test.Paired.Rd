\name{Test.Paired}
\alias{Test.Paired}
\title{Test Paired Data Sets}
\description{Tests two paired data sets for similarity.}

\usage{Test.Paired(group.data, numPerms = 1000, parallel = FALSE, cores = 3)}

\arguments{
  \item{group.data}{A list of 2 matrices of taxonomic counts(columns) for each sample(rows).}
  \item{numPerms}{Number of permutations.  In practice this should be at least 1,000.}
  \item{parallel}{When this is 'TRUE' it allows for parallel calculation of the permutations. Requires the package \code{doParallel}.}
  \item{cores}{The number of parallel processes to run if parallel is 'TRUE'.}
}

\value{A pvalue.}

\author{Sharina Carter, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	
	
	### Since saliva and throat come from same subjects, the data is paired 
	saliva1 <- saliva[-24,] # Make saliva 23 subjects to match throat
	group.data <- list(throat, saliva1)
	
	### We use 1 for speed, should be at least 1,000
	numPerms <- 1
	
	pval <- Test.Paired(group.data, numPerms)
	pval
}