\name{MC.ZT.statistics}
\alias{MC.ZT.statistics}
\title{Size and Power of Goodness of Fit Test: Multinomial vs. Dirichlet-Multinomial}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the Multinomial vs. Dirichlet-Multinomial goodness of fit test, using the 
	C(\eqn{\alpha})-optimal test statistics of Kim and Margolin (1992) (t statistics) and the C(\eqn{\alpha})-optimal test statistics of (Paul et al., 1989).
}

\usage{MC.ZT.statistics(Nrs, numMC = 10, fit, type = "ha", siglev = 0.05, MC = NULL)}

\arguments{
  \item{Nrs}{A vector specifying the number of reads/sequence depth for each sample.}
  \item{numMC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{fit}{A list (in the format of the output of dirmult function) containing the data parameters for evaluating either the size or power of the test.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
  \item{MC}{Deprecated. Replaced with numMC for clarity.}
}

\value{A vector containing both the size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test for both the z and t statistics.}

\details{
	Note: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
}

\examples{
	data(saliva) 
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- DM.MoM(saliva) 
	
	### Set up the number of Monte-Carlo experiments
	### We use 1 for speed, should be at least 1,000
	numMC <- 1
	
	### Generate the number of reads per sample
	### The first number is the number of reads and the second is the number of subjects
	nrs <- rep(15000, 25)
	
	### Computing size of the test statistics (Type I error)
	pval1 <- MC.ZT.statistics(nrs, numMC, fit.saliva, "hnull") 
	pval1
	
	### Computing Power of the test statistics (Type II error)
	pval2 <- MC.ZT.statistics(nrs, numMC, fit.saliva)
	pval2
}