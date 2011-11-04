\name{MC.Xoc.statistics}
\alias{MC.Xoc.statistics}

\title{Size and Power of Several Sample-Overdispersion Test Comparisons}
\description{This Monte-Carlo simulation procedure provides the power and size of the several sample-overdispersion 
test comparison, using the likelihood-ratio-test statistics.
}
\usage{MC.Xoc.statistics(Nrs, MC, group.alphap, n.groups, 
	type = "hnull", siglev = 0.05)}

\arguments{
  \item{Nrs}{
A vector specifying the number of reads or sequence depth for each sample.}
  \item{MC}{
Number of Monte-Carlo experiments.}
  \item{group.alphap}{
If 'hull': A vector of alpha parameters for each taxa.\cr
If 'ha': A list consisting of vectors of alpha parameters for each taxa.}
  \item{n.groups}{
The number of groups to compare.}
  \item{type}{
If 'hnull': Computes the size of the test.\cr
If 'ha': Computes the power of the test.}
  \item{siglev}{
Significance level, default 0.05.}
}

\value{
Effect size (under 'hnull') or power (under 'ha') of the test.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 
data(tonsils)

### Get a list of dirichlet-multinomial parameters for the data
fit.saliva <- dirmult(saliva) 
fit.tonsils <- dirmult(tonsils) 

### Number of samples per group
P <- 25

### Generate a random vector of number of reads per sample
q <- seq(5000, 20000, by=50)
Nrs <- sample(q, size=P)

### Computing size of the test statistics (Type I error)
MyMCHn <- MC.Xoc.statistics(Nrs, 1, fit.tonsils$gamma, 2, "hnull")
MyMCHn

### Computing Power of the test statistics (Type II error)
MyMCHa <- MC.Xoc.statistics(Nrs, 1, list(fit.tonsils$gamma, 
	fit.saliva$gamma), 2, "ha")
MyMCHa
}