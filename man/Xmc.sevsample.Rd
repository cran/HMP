\name{Xmc.sevsample}
\alias{Xmc.sevsample}

\title{Generalized Wald-type Statistics: Several sample RAD probability-mean test comparison with known common vector.}
\description{
This function computes the Generalized Wald-type test statistic (Wilson and Koehler, 1984) and corresponding p-value to assess whether the sample 
RAD probability-means from multiple populations are the same or different. The statistics assumes that a common RAD probability-mean vector for comparison under the null 
hypothesis is known.}
\usage{Xmc.sevsample(group.data, pi0)}

\arguments{
  \item{group.data}{
A list of matrices of data which contain samples and their taxa counts (matrices should have the same column names).}
  \item{pi0}{
The RAD-probability mean vector.}
}

\value{A list containing the Generalized Wald-type statistics and \eqn{P} vlaues.}
\references{
Wilson, J. R., and Koehler, K. J. (1984). Testing of equality of vectors of proportions for several cluster samples. 
Proceedings of Joint Statistical Association Meetings. Survey Research Methods.}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 
data(throat)
data(tonsils)

### Get a list of dirichlet-multinomial parameters for the data
fit.saliva <- dirmult(saliva) 
mygroup <- list(throat[,1:15], tonsils[,1:15])

Xmc.sevsample_check <- Xmc.sevsample(mygroup, fit.saliva$pi)
Xmc.sevsample_check
}