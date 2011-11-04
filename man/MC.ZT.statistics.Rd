\name{MC.ZT.statistics}
\alias{MC.ZT.statistics}

\title{Size and Power of Goodness of Fit Test: Multinomial vs. Dirichlet-Multinomial}
\description{This Monte-Carlo simulation procedure provides the power and size of the Multinomial vs. Dirichlet-Multinomial goodness of fit test, using the 
C(\eqn{\alpha})-optimal test statistics of Kim and Margolin (1992) (t statistics) and the C(\eqn{\alpha})-optimal test statistics of (Paul et al., 1989).}
\usage{MC.ZT.statistics(Nrs, MC, fit, type = "hnull", siglev = 0.05)}

\arguments{
  \item{Nrs}{
A vector specifying the number of reads or sequence depth for each sample.}
  \item{MC}{
Number of Monte-Carlo experiments.}
  \item{fit}{
A list (in the format of the output of dirmult function) containing the data parameters for evaluating either the size or power of the test.}
  \item{type}{
If 'hnull': Computes the size of the test.\cr
If 'ha': Computes the power of the test.}
  \item{siglev}{
Significance level for size of the test / power calculation. The default is 0.05.}
}

\value{
Effect size (under 'hnull') or power (under 'ha') of the test.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 

### Get a list of dirichlet-multinomial parameters for the data
fit.saliva <- dirmult(saliva) 

### Number of samples per group
P <- 25

### Generate a random vector of number of reads per sample
q <- seq(5000, 20000, by=50)
Nrs <- sample(q, size=P)

### Computing size of the test statistics (Type I error)
MC_ZT_check1 <- MC.ZT.statistics(Nrs, 1, fit.saliva, "hnull") 
MC_ZT_check1 

### Computing Power of the test statistics (Type II error)
MC_ZT_check2 <- MC.ZT.statistics(Nrs, 1, fit.saliva, "ha")
MC_ZT_check2
}