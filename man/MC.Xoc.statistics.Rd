\name{MC.Xoc.statistics}
\alias{MC.Xoc.statistics}
\title{Size and Power of Several Sample-Overdispersion Test Comparisons}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the several sample-overdispersion 
	test comparison, using the likelihood-ratio-test statistics.
}

\usage{
	MC.Xoc.statistics(group.Nrs, numMC = 10, group.alphap, type = "ha", siglev = 0.05)
}

\arguments{
  \item{group.Nrs}{A list specifying the number of reads/sequence depth for each sample in a group with one group per list entry.}
  \item{numMC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{group.alphap}{
		If \code{"hnull"}: A vector of alpha parameters for each taxa.\cr
		If \code{"ha"}: A list consisting of vectors of alpha parameters for each taxa.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
}

\value{Size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test.}

\details{
	\enumerate{
		\item Note 1: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
		\item Note 2: All components of \code{group.alphap} should be non-zero or it may result in errors and/or invalid results.
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
	alphap <- fit.tonsils$gamma
	pval1 <- MC.Xoc.statistics(group.Nrs, numMC, alphap, "hnull")
	pval1
	
	\dontrun{
		### Computing Power of the test statistics (Type II error)
		alphap <- rbind(fit.saliva$gamma, fit.throat$gamma, fit.tonsils$gamma)
		pval2 <- MC.Xoc.statistics(group.Nrs, numMC, alphap, "ha")
		pval2
	}
}