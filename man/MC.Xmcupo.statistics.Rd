\name{MC.Xmcupo.statistics}
\alias{MC.Xmcupo.statistics}
\title{Size and Power of Several Sample RAD-Probability Mean Test Comparisons: Unknown Vector of Proportion}
\description{
	This Monte-Carlo simulation procedure provides the power and size of the several sample RAD-probability mean test 
	comparisons without reference vector of proportions, using the Generalized Wald-type statistics.
}

\usage{MC.Xmcupo.statistics(Nrs, MC, pi0, group.pi, group.theta, type = "ha", siglev = 0.05)}

\arguments{
  \item{Nrs}{A list specifying the number of reads/sequence depth for each sample in a group with one group per list entry.}
  \item{MC}{Number of Monte-Carlo experiments.  In practice this should be at least 1,000.}
  \item{pi0}{The RAD-probability mean vector.}
  \item{group.pi}{
		If \code{"hnull"}: This argument is ignored. \cr
		If \code{"ha"}: A matrix where each row is a vector pi values for each group.}
  \item{group.theta}{A vector of overdispersion values for each group.}
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
	Nrs1 <- rep(12000, 10)
	Nrs2 <- rep(12000, 19)
	Nrs3 <- rep(12000, 19)
	group.Nrs <- list(Nrs1, Nrs2, Nrs3)
	
	### Computing size of the test statistics (Type I error)
	group.theta <- c(fit.throat$theta, fit.tonsils$theta)
	mc.xmcupo_check1 <- MC.Xmcupo.statistics(group.Nrs, MC, 
			fit.saliva$pi, group.theta=group.theta, type="hnull")
	mc.xmcupo_check1
	
	### Computing Power of the test statistics (Type II error)
	pi_2grp <- rbind(fit.throat$pi, fit.tonsils$pi)
	mc.xmcupo_check2 <- MC.Xmcupo.statistics(group.Nrs, MC, 
			fit.saliva$pi, pi_2grp, group.theta)
	mc.xmcupo_check2
}