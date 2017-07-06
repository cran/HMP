\name{Est.PI}
\alias{Est.PI}
\title{Estimate the Pi Vector}

\description{
	Calculates Dirichlet-Multinomial parameters for every group using Maximum 
	Likelihood and Method of Moments estimates: Taxa proportion estimates (PI vector) 
	with standard errors and Confidence intervals, as well as theta values with 
	standard errors.
}

\usage{Est.PI(group.data, conf = .95)}

\arguments{
  \item{group.data}{A list of matrices of taxonomic counts(columns) for each sample(rows).}
  \item{conf}{The desired confidence limits. The default is 95\%}
}

\value{
	A list containing the parameters: PI, SE and the upper/lower bounds of the confidence interval for every taxa, 
	and the theta values with standard errors for both MLE and MOM.
}

\author{Sharina Carter, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	\dontrun{
		data(saliva)
		data(throat)
		data(tonsils)
		
		### Combine the data sets into a single list
		group.data <- list(saliva, throat, tonsils)
		
		### Get PI using MLE and MOM with CI
		piEsts <- Est.PI(group.data)
		
		mle <- piEsts$MLE
		mom <- piEsts$MOM
	}
}