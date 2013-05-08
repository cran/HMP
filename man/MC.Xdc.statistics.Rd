\name{MC.Xdc.statistics}
\alias{MC.Xdc.statistics}
\title{Size and Power for the Several-Sample DM Parameter Test Comparison}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the several sample Dirichlet-Multinomial parameter test comparison, 
	using the likelihood-ratio-test statistics.
}

\usage{MC.Xdc.statistics(Nrs, MC, alphap, n.groups, type = "ha", siglev = 0.05, est = "mom")}

\arguments{
  \item{Nrs}{A list specifying the number of reads/sequence depth for each sample in a group with one group per list entry.}
  \item{MC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{alphap}{
		If \code{"hnull"}: A matrix where rows are vectors of alpha parameters for each group. \cr
		If \code{"ha"}: A matrix consisting of vectors of alpha parameters for each taxa in each group.}
  \item{n.groups}{The number of groups to compare.}
  \item{type}{
		If \code{"hnull"}: Computes the size of the test.\cr
		If \code{"ha"}: Computes the power of the test. (default)}
  \item{siglev}{Significance level for size of the test / power calculation. The default is 0.05.}
  \item{est}{
		The type of parameter estimator to be used with the Likelihood-ratio-test statistics, 'mle' or 'mom'. Default value is 'mom'.  (See Note 2 in details)}
}

\value{Size of the test statistics (under \code{"hnull"}) or power (under \code{"ha"}) of the test.}

\details{
	\enumerate{
		\item Note 1: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
		\item Note 2: 'mle' will take significantly longer time and may not be optimal for small sample sizes; 'mom' will provide a more conservative result in such a case.
		\item Note 3: All components of \code{alphap} should be non-zero or it may result in errors and/or invalid results.
	}
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
	### We set MC=1 due to CRAN restrictions, Please set MC to be at least 1,000
	MC <- 1 
	
	### Generate a random vector of number of reads per sample
	Nrs1 <- rep(12000, 9)
	Nrs2 <- rep(12000, 11)
	Nrs3 <- rep(12000, 12)
	group.Nrs <- list(Nrs1, Nrs2, Nrs3)
	
	### Computing size of the test statistics (Type I error)
	mc.xdc_check1 <- MC.Xdc.statistics(group.Nrs, MC, fit.saliva$gamma, 3, "hnull", .05, "mom")
	mc.xdc_check1
		
	### Computing Power of the test statistics (1 - Type II error)
	group.alphap <- rbind(fit.saliva$gamma, fit.throat$gamma, fit.tonsils$gamma)
	mc.xdc_check2 <- MC.Xdc.statistics(group.Nrs, MC, group.alphap, 3, "ha", 0.05, "mom")
	mc.xdc_check2
}