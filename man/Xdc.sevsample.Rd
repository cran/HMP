\name{Xdc.sevsample}
\alias{Xdc.sevsample}
\title{Likelihood-Ratio-Test Statistics: Several Sample Dirichlet-Multinomial Test Comparison}
\description{
	This routine provides the value of the Likelihood-Ratio-Test Statistics and the corresponding p-value for evaluating the several 
	sample Dirichlet-Multinomial parameter test comparison.
}
	
\usage{Xdc.sevsample(group.data, epsilon = 10^(-4), est = "mom")}

\arguments{
  \item{group.data}{A list where each element is a matrix of taxonomic counts(columns) for each sample(rows). (See Notes 1 and 2 in details)}
  \item{epsilon}{Convergence tolerance. To terminate, the difference between two succeeding log-likelihoods must be smaller than epsilon. Default value is 10^(-4).}
  \item{est}{
		The type of parameter estimator to be used with the Likelihood-ratio-test statistics, 'mle' or 'mom'. Default value is 'mom'.  (See Note 3 in details)}
}

\value{A list containing the Xdc statistics and p-value.}

\details{
	To assess whether the Dirichlet parameter vector, \eqn{\mathbf{\alpha}_{\mathrm{m}}=\mathbf{\pi}_{\mathrm{m}} \frac{1-\theta_{\mathrm{m}}}{\theta_{\mathrm{m}}}}(a  function of the RAD probability-mean vector  and overdispersion), observed in \eqn{J} groups  of microbiome samples are equal to each other, the following hypothesis 
	\eqn{\mathrm{H}_{\mathrm{o}}: \mathbf{\alpha}_{\mathrm{1}} = \cdots =\mathbf{\alpha}_{\mathrm{m}}=\cdots= \mathbf{\alpha}_{\mathrm{J}}=\mathbf{\alpha}_{\mathrm{o}}} 
	versus \eqn{\mathrm{H}_{\mathrm{a}}:  \mathbf{\alpha}_{\mathrm{m}} \ne \mathbf{\alpha}_{\mathrm{o}}, m=1, \ldots, J} can be tested. The null hypothesis implies that the HMP samples across groups have the same mean and overdispersion, indicating that the RAD models are identical. In particular, the likelihood-ratio test statistic is used, which is given by, 
	\deqn{x_{\mathrm{dc}}=-2 \log\left\{\frac{L\left(\mathbf{\alpha}_{\mathrm{o}}; \mathbf{X}_{\mathrm{1}},\ldots, \mathbf{X}_{\mathrm{J}} \right)}{L\left(\mathbf{\alpha}_{\mathrm{1}},\ldots,\mathbf{\alpha}_{\mathrm{J}}; \mathbf{X}_{\mathrm{1}},\ldots, \mathbf{X}_{\mathrm{J}} \right)}\right\}.} 
	The asymptotic null distribution of \eqn{x_{\mathrm{dc}}} follows a Chi-square with degrees of freedom equal to (J-1)*K, where K is the number of taxa (Wilks, 1938).
	
	\enumerate{
		\item Note 1: The matrices in \code{group.data} must contain the same taxa, in the same order.
		\item Note 2: Each taxa should be present in at least 1 sample, a column with all 0's may result in errors and/or invalid results.
		\item Note 3: 'mle' will take significantly longer time and may not be optimal for small sample sizes; 'mom' will provide more conservative results in such a case.
	}
}

\references{Wilks, S. S. (1938). The Large-Sample Distribution of the Likelihood Ratio for Testing Composite Hypotheses. The Annals of Mathematical Statistics 9, 60-62.}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva) 
	data(throat)
	
	### Change the number of display digits so our output looks better
	defaultd <- .Options$digits
	options(digits=5)
	
	xdc.sev_check <- Xdc.sevsample(list(saliva, throat))
	xdc.sev_check
	
	options(digits=defaultd) ### Set the number of digits back for the user
}