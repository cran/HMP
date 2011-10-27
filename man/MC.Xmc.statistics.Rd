\name{MC.Xmc.statistics}
\alias{MC.Xmc.statistics}
\title{Size and Power of Several Sample RAD-Probability Mean Test Comparison}
\description{This Monte-Carlo simulation procedure provides the power and size of the several sample RAD-probability mean test 
comparison with known reference vector of proportions using the Generalized Wald-type statistics.}

\usage{MC.Xmc.statistics(Nrs, MC, pi0, group.pi, group.theta, 
	type = "hnull", siglev = 0.05)}

\arguments{
  \item{Nrs}{
A vector specifying the number of reads or sequence depth for each sample.}
  \item{MC}{
Number of Monte-Carlo experiments.}
  \item{pi0}{
The RAD-probability mean vector.}
  \item{group.pi}{
A matrix where each row is a vector pi values for each group.}
  \item{group.theta}{
A vector of overdispersion values for each group.}
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
group.theta <- c(0.01,0.05)
MC.Xmc_chck1 <- MC.Xmc.statistics(Nrs, 1, fit.saliva$pi, , group.theta, "hnull")
MC.Xmc_chck1

### Computing Power of the test statistics (Type II error)
pi_2gr <- rbind(fit.throat$pi, fit.tonsils$pi)
MC.Xmc_chck2 <- MC.Xmc.statistics(Nrs, 1, fit.saliva$pi, pi_2gr, group.theta, "ha")
MC.Xmc_chck2
}