\name{formatDataSets}
\alias{formatDataSets}
\title{Format Data}
\description{
	For a list of datasets, this function finds the union of taxa across all datasets and 
	transforms them such that they all have the same columns of taxa.
}

\usage{formatDataSets(group.data, data)}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows). 
  		Note that the row names should correspond to sample names}
  \item{data}{Deprecated. Replaced with group.data for clarity.}
 }
 
\details{
	This function will also sort all the columns into the same order for every 
	dataset and remove any columns that have 0's for every sample.
	
	E.g. For two datasets, any taxa present in dataset1 but not dataset2 will be 
	added to dataset2 with a 0 count for all samples and vice versa.
}

\value{The list given, but modified so every data set has the same ordering and number of columns}

\examples{
	data(saliva)
	data(throat)
	
	### Set each data set to have 10 different columns
	saliva2 <- saliva[,1:10]
	throat2 <- throat[,11:20]
	
	### Combine the data sets into a single list
	group.data <- list(saliva2, throat2)
	
	formattedData <- formatDataSets(group.data)
	formattedData[[1]][1:5, 1:5]
}
