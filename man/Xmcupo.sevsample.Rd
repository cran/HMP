\name{Xmcupo.sevsample}
\alias{Xmcupo.sevsample}

\title{Generalized Wald-type Statistics: Several sample RAD probability-mean test comparison with unknown common vector.}
\description{This function computes the Generalized Wald-type test statistic (Wilson and Koehler, 1984) 
and corresponding p-value to assess whether the sample RAD probability-means from multiple populations are same or different. 
The statistics assumes that a common RAD probability-mean vector for comparison under the null hypothesis is unknown.}
\usage{
Xmcupo.sevsample(group.data, K)
}

\arguments{
  \item{group.data}{
A list of matrices of data which contain samples and their taxa counts (matrices should have the same column names).}
  \item{K}{
The number of taxa.}
}

\value{A list containing the Generalized Wald-type statistics and \eqn{P} values.}
\references{
Wilson, J. R., and Koehler, K. J. (1984). Testing of equality of vectors of proportions for several cluster samples. 
Proceedings of Joint Statistical Association Meetings. Survey Research Methods.
}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 
data(tonsils)
data(throat)

mygroup <- list(throat[,1:15], tonsils[,1:15], saliva[,1:15])
Xmcupo_check <- Xmcupo.sevsample(mygroup, 15)
Xmcupo_check
}