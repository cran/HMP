\name{Plot.MDS}
\alias{Plot.MDS}
\title{Multidimensional Scaling Plot of Microbiome Data}
\description{Plots any number of data sets on an MDS plot.}

\usage{Plot.MDS(group.data, main = "Group MDS", retCords = FALSE)}

\arguments{
  \item{group.data}{A list of matrices of taxonomic counts(columns) for each sample(rows).}
  \item{main}{A string to be used as the plots title.}
  \item{retCords}{A boolean to return the mds coordinates or not.}
}

\value{A MDS plot and possibly the x-y coordinates for every point.}

\examples{
	data(saliva)
	data(throat)
	data(tonsils)
	
	### Combine the data sets into a single list
	group.data <- list(saliva, throat, tonsils)
	
	Plot.MDS(group.data)
}