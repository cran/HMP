\name{MC.Xdc.statistics}
\alias{MC.Xdc.statistics}
\title{Size and Power for the Several-Sample DM Parameter Test Comparison}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the several sample Dirichlet-Multinomial parameter test comparison, 
	using the likelihood-ratio-test statistics.
}

\usage{
	MC.Xdc.statistics(group.Nrs, numMC = 10, alphap, type = "ha", siglev = 0.05, 
	est = "mom", MC = NULL, Nrs = NULL)
}

\arguments{
  \item{group.Nrs}{A list specifying the number of reads/sequence depth for each sample in a group with one group per list entry.}
  \item{numMC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{alphap}{
		If \code{"hnull"}: A matrix where rows are vectors of alpha parameters for the reference group. \cr
		If \code{"ha"}: A matrix consisting of vectors of alpha parameters for each taxa in each group.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
  \item{est}{
		The type of parameter estimator to be used with the Likelihood-ratio-test statistics, 'mle' or 'mom'. Default value is 'mom'.  (See Note 2 in details)}
  \item{MC}{Deprecated. Replaced with numMC for clarity.}
  \item{Nrs}{Deprecated. Replaced with group.Nrs for clarity.}
}

\value{Size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test.}

\details{
	\enumerate{
		\item Note 1: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
		\item Note 2: 'mle' will take significantly longer time and may not be optimal for small sample sizes; 'mom' will provide a more conservative result in such a case.
		\item Note 3: All components of \code{alphap} should be non-zero or it may result in errors and/or invalid results.
	}
}

\examples{
	data(saliva)
	data(throat)
	data(tonsils)
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- DM.MoM(saliva) 
	fit.throat <- DM.MoM(throat)
	fit.tonsils <- DM.MoM(tonsils)
	
	### Set up the number of Monte-Carlo experiments
	### We use 1 for speed, should be at least 1,000
	numMC <- 1
	
	### Generate the number of reads per sample
	### The first number is the number of reads and the second is the number of subjects
	nrsGrp1 <- rep(12000, 9)
	nrsGrp2 <- rep(12000, 11)
	nrsGrp3 <- rep(12000, 12)
	group.Nrs <- list(nrsGrp1, nrsGrp2, nrsGrp3)
	
	### Computing size of the test statistics (Type I error)
	alphap <- fit.saliva$gamma
	pval1 <- MC.Xdc.statistics(group.Nrs, numMC, alphap, "hnull")
	pval1
	
	### Computing Power of the test statistics (Type II error)
	alphap <- rbind(fit.saliva$gamma, fit.throat$gamma, fit.tonsils$gamma)
	pval2 <- MC.Xdc.statistics(group.Nrs, numMC, alphap)
	pval2
}