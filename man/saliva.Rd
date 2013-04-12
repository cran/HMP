\name{saliva}
\alias{saliva}
\docType{data}
\title{Saliva Data Set}
\description{
	The saliva data set formed by the Ranked-abundance distribution vectors of 24 subjects. 
	The RAD vectors contains 21 elements formed by the 20 most abundant taxa at the genus level and additional 
	taxa containing the sum of the remaining less abundant taxa per sample. Note that the incorporation of the 
	additional taxon (taxon 21) in the analysis allows for estimating the RAD proportional-mean of taxa with respect 
	to all the taxa within the sample.
}

\usage{data(saliva)}

\format{The format is a matrix of 24 rows by 21 columns, with the each row being a separate subject and each column being a different taxa.}

\examples{
	data(saliva)
}

\keyword{datasets}