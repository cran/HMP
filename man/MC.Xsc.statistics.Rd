\name{MC.Xsc.statistics}
\alias{MC.Xsc.statistics}
\title{Size and Power for the One Sample RAD Probability-Mean Test Comparison}
\description{This Monte-Carlo simulation procedure provides the power and size of the one sample RAD probability-mean test, using the Generalized Wald-type statistic.}

\usage{MC.Xsc.statistics(Nrs, MC, fit, pi0 = NULL, type = "ha", siglev = 0.05)}

\arguments{
  \item{Nrs}{A vector specifying the number of reads/sequence depth for each sample.}
  \item{MC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{fit}{A list (in the format of the output of dirmult function) containing the data parameters for evaluating either the size or power of the test.}
  \item{pi0}{The RAD-probability mean vector. If the type is set to \code{"hnull"} then pi0 is set by the sample in fit.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
}

\value{Size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test.}

\details{
	Note: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat) 
	data(tonsils)
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- DM.MoM(saliva) 
	fit.throat <- DM.MoM(throat)
	fit.tonsils <- DM.MoM(tonsils) 
	
	### Set up the number of Monte-Carlo experiments
	### We use 1 for speed, should be around 10,000
	MC <- 1
	
	### Generate a random vector of number of reads per sample
	Nrs <- rep(15000, 25)
	
	### Computing size of the test statistics (Type I error)
	mc.xsc_check1 <- MC.Xsc.statistics(Nrs, MC, fit.tonsils, fit.saliva$pi, "hnull")
	mc.xsc_check1
	
	### Computing Power of the test statistics (Type II error)
	mc.xsc_check2 <- MC.Xsc.statistics(Nrs, MC, fit.throat, fit.tonsils$pi)
	mc.xsc_check2
}