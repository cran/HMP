\name{formatDataSets}
\alias{formatDataSets}
\title{Format Data}
\description{
	For a list of datasets, this function finds the union of taxa across all datasets and 
	transforms them such that they all have the same columns of taxa.
}

\usage{formatDataSets(data)}

\arguments{
  \item{data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows).}
 }
 
\details{
	This function will also sort all the columns into the same order for every 
	dataset and remove any columns that have 0's for every sample.
	
	E.g. For two datasets, any taxa present in dataset1 but not dataset2 will be 
	added to dataset2 with a 0 count for all samples and vice versa.
}

\value{The list given, but modified so every data set has the same ordering and number of columns}

\author{Sharina Carter, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat)
	
	saliva2 <- saliva[,1:10]
	throat2 <- throat[,11:20]
	
	mygroup <- list(saliva2, throat2)
	newgroup <- formatDataSets(mygroup)
}
