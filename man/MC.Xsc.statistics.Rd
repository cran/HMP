\name{MC.Xsc.statistics}
\alias{MC.Xsc.statistics}

\title{Size and Power for the One Sample RAD Probability-Mean Test Comparison}
\description{This Monte-Carlo simulation procedure provides the power and size of the one sample RAD probability-mean test, using the Generalized Wald-type statistic.}
\usage{MC.Xsc.statistics(Nrs, MC, fit, pi0 = NULL, 
	type = "hnull", siglev = 0.05)}

\arguments{
  \item{Nrs}{
A vector specifying the number of reads or sequence depth for each sample.}
  \item{MC}{
Number of Monte-Carlo experiments.}
  \item{fit}{
A list (in the format of the output of dirmult function) containing the data parameters for evaluating either the size or power of the test.}
  \item{pi0}{
The RAD-probability mean vector pi0. If pi0=NULL then pi0 is given by the RAD-probability mean of the sample in fit.}
  \item{type}{
If 'hnull': Computes the size of the test.\cr
If 'ha': Computes the power of the test.}
  \item{siglev}{
Significance level, the default is 0.05.}
}
\value{
Effect size (under 'hnull') or power (under 'ha') of the test.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva)
data(throat) 
data(tonsils)

### Get a list of dirichlet-multinomial parameters for the data
fit.saliva <- dirmult(saliva) 
fit.throat <- dirmult(throat)
fit.tonsils <- dirmult(tonsils)

### Number of samples per group
P <- 25

### Generate a random vector of number of reads per sample
q <- seq(5000, 20000, by=50)
Nrs <- sample(q, size=P)

### Computing size of the test statistics (Type I error)
mcXsc1_check1 <- MC.Xsc.statistics(Nrs, 1, fit.tonsils, fit.saliva$pi, "hnull", 0.05)
mcXsc1_check1

### Computing Power of the test statistics (Type II error)
mcXsc1_check2 <- MC.Xsc.statistics(Nrs, 1, fit.throat,  fit.tonsils$pi, "ha", 0.01)
mcXsc1_check2
}