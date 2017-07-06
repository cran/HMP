\name{DM.Rpart.Perm}
\alias{DM.Rpart.Perm}
\title{Dirichlet-Multinomial RPart Permutations}
\description{Uses custom rpart methods to find the optimal tree splits.}

\usage{
	DM.Rpart.Perm(data, covars, plot = TRUE,  numPerms = 1000, parallel = FALSE, 
	cores = 3, minsplit = 1, minbucket = 1, cp = 0)
}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
  \item{covars}{A matrix of covariates(columns) for each sample(rows).}
  \item{plot}{When 'TRUE' a tree plot of the results will also be generated.}
  \item{numPerms}{Number of permutations.  In practice this should be at least 1,000.}
  \item{parallel}{When this is 'TRUE' it allows for parallel calculation of the permutations. Requires the package \code{doParallel}.}
  \item{cores}{The number of parallel processes to run if parallel is 'TRUE'.}
  \item{minsplit}{
  		The minimum number of observations to split on, see \link[rpart]{rpart.control}. 
  		This should generally stay at 1.
  	}
  \item{minbucket}{
  		The minimum number of observations in any terminal node, see \link[rpart]{rpart.control}. 
  		This should generally stay at 1.
  	}
  \item{cp}{
  		The complexity parameter, see \link[rpart]{rpart.control}.
  		This should generally stay at 0.
  	}
}

\value{A list containing the raw rpart tree, the pvalues for different splits, and data from the rpart permuting.}

\author{Sharina Carter, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	data(tonsils)
	
	### Create some covariates for our data set
	site <- c(rep("Saliva", nrow(saliva)), rep("Throat", nrow(throat)), 
			rep("Tonsils", nrow(tonsils)))
	covars <- data.frame(Group=site)
	
	### Combine our data into a single object
	data <- rbind(saliva, throat, tonsils)
	
	### We use 1 for speed, should be at least 1,000
	numPerms <- 1
	
	rpartPerm <- DM.Rpart.Perm(data, covars, numPerms=numPerms)
	
	### Pull out a tree
	rpartPerm$pvals
	selectedNum <- 3
	bestTree <- rpart::prune(rpartPerm$rawTree, cp=rpartPerm$rawPrune[selectedNum, 2])
	
	### Plot the best tree
	rpart.plot::rpart.plot(bestTree, main="Best Tree", extra=1)
}
