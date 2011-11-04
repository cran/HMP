\name{MC.Xdc.statistics}
\alias{MC.Xdc.statistics}
\title{Size and Power for the Several-Sample DM Parameter Test Comparison}
\description{
This Monte-Carlo simulation procedure provides the power and size of the several sample Dirichlet-Multinomial parameter test comparison, 
using the likelihood-ratio-test statistics.}
\usage{
MC.Xdc.statistics(Nrs, MC, alphap, n.groups, 
	type = "hnull", siglev = 0.05, Est="MLE")
}

\arguments{
  \item{Nrs}{
If 'hnull': A vector specifying the number of reads or sequence depth for each sample in the group.\cr
If 'ha': A vector specifying the number of reads or sequence depth for each sample in all groups.}
  \item{MC}{
Number of Monte-Carlo experiments.}
  \item{alphap}{
If 'hnull': A matrix where rows are vectors of alpha parameters for each group. \cr
If 'ha': A matrix consisting of vectors of alpha parameters for each taxa in each group.}
  \item{n.groups}{
The number of groups to compare.}
  \item{type}{
If 'hnull': Computes the size of the test. \cr
If 'ha': Computes the power of the test.}
  \item{siglev}{
Significance level, default value is 0.05.}
  \item{Est}{
The type of parameter estimators to be used with the Likelihood-ratio-test statistics i.e., 'MLE' or 'MoM'. Default value is 'MLE', however, 
to get a quick estimation of the size and power the LRT test, the 'MoM' can be used.}
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
MCXDC_chck1 <- MC.Xdc.statistics(Nrs, 1, fit.saliva$gamma, 3, "hnull")
MCXDC_chck1

### Generate 3 random vectors of number of reads per sample
Nrs1 <- sample(q, size=P)
Nrs2 <- sample(q, size=P)
Nrs3 <- sample(q, size=P)
group.Nrs <- rbind(Nrs1, Nrs2, Nrs3)

### Computing Power of the test statistics (Type II error)
group.alphap <- rbind(fit.saliva$gamma, fit.throat$gamma, fit.tonsils$gamma)
MCXDC_chck2 <- MC.Xdc.statistics(group.Nrs, 1, group.alphap, 3, "ha", 0.01)
MCXDC_chck2
}