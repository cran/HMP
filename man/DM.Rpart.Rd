\name{DM.Rpart}
\alias{DM.Rpart}
\title{Dirichlet-Multinomial RPart}
\description{Uses custom recursive partitioning (rpart) methods to find groups based on the Dirichlet-Multinomial distribution.}

\usage{DM.Rpart(data, covars, plot = TRUE, main = "", minsplit = 1, minbucket = 1, cp = 0)}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
  \item{covars}{A matrix of covariates(columns) for each sample(rows).}
  \item{plot}{When 'TRUE' a tree plot of the results will be generated.}
  \item{main}{A string to be used as the plot title.}
  \item{minsplit}{The minimum number of observations to split on, see \link[rpart]{rpart.control}.}
  \item{minbucket}{The minimum number of observations in any terminal node, see \link[rpart]{rpart.control}.}
  \item{cp}{The complexity parameter, see \link[rpart]{rpart.control}.}
}

\value{An rpart object and a plot of the tree created if plot = 'TRUE'}

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
	
	rpartRes <- DM.Rpart(data, covars)
}
