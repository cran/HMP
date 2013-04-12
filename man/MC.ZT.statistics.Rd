\name{MC.ZT.statistics}
\alias{MC.ZT.statistics}
\title{Size and Power of Goodness of Fit Test: Multinomial vs. Dirichlet-Multinomial}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the Multinomial vs. Dirichlet-Multinomial goodness of fit test, using the 
	C(\eqn{\alpha})-optimal test statistics of Kim and Margolin (1992) (t statistics) and the C(\eqn{\alpha})-optimal test statistics of (Paul et al., 1989).
}

\usage{MC.ZT.statistics(Nrs, MC, fit, type = "ha", siglev = 0.05)}

\arguments{
  \item{Nrs}{A vector specifying the number of reads/sequence depth for each sample.}
  \item{MC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{fit}{A list (in the format of the output of dirmult function) containing the data parameters for evaluating either the size or power of the test.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
}

\value{A vector containing both the size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test for both the z and t statistics.}

\details{
	Note: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva) 
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- dirmult(saliva) 
	
	### Set up the number of Monte-Carlo experiments
	### We set MC=1 due to CRAN restrictions, Please set MC to be at least 1,000
	MC <- 1 
	
	### Generate a random vector of number of reads per sample
	Nrs <- rep(15000, 25)
	
	### Computing size of the test statistics (Type I error)
	mc.zt_check1 <- MC.ZT.statistics(Nrs, MC, fit.saliva, "hnull") 
	mc.zt_check1
	
	### Computing Power of the test statistics (1 - Type II error)
	mc.zt_check2 <- MC.ZT.statistics(Nrs, MC, fit.saliva, "ha")
	mc.zt_check2
}