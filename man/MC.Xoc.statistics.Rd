\name{MC.Xoc.statistics}
\alias{MC.Xoc.statistics}
\title{Size and Power of Several Sample-Overdispersion Test Comparisons}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the several sample-overdispersion 
	test comparison, using the likelihood-ratio-test statistics.
}

\usage{MC.Xoc.statistics(Nrs, MC, group.alphap, n.groups, type = "ha", siglev = 0.05)}

\arguments{
  \item{Nrs}{A list specifying the number of reads/sequence depth for each sample in a group with one group per list entry.}
  \item{MC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{group.alphap}{
		If \code{"hnull"}: A vector of alpha parameters for each taxa.\cr
		If \code{"ha"}: A list consisting of vectors of alpha parameters for each taxa.}
  \item{n.groups}{The number of groups to compare.}
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

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	data(throat) 
	data(tonsils)
	
	### Get a list of dirichlet-multinomial parameters for the data
	fit.saliva <- dirmult(saliva) 
	fit.throat <- dirmult(throat)
	fit.tonsils <- dirmult(tonsils) 
	
	### Set up the number of Monte-Carlo experiments
	### We set MC=1 due to CRAN restrictions, Please set MC to be at least 1,000
	MC <- 1 
	
	### Generate a random vector of number of reads per sample
	Nrs1 <- rep(12000, 10)
	Nrs2 <- rep(12000, 11)
	Nrs3 <- rep(12000, 9)
	group.Nrs <- list(Nrs1, Nrs2, Nrs3)
	
	### Computing size of the test statistics (Type I error)
	mc.xoc_check1 <- MC.Xoc.statistics(group.Nrs, MC, fit.tonsils$gamma, 3, "hnull")
	mc.xoc_check1 
	
	### Computing Power of the test statistics (1 - Type II error)
	group.alphap <- rbind(fit.saliva$gamma, fit.throat$gamma, fit.tonsils$gamma)
	mc.xoc_check2 <- MC.Xoc.statistics(group.Nrs, MC, group.alphap, 3, "ha")
	mc.xoc_check2
}