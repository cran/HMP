\name{Xoc.sevsample}
\alias{Xoc.sevsample}

\title{Likelihood-Ratio-Test Statistics: Several Sample Overdispersion Test Comparison}
\description{This routine provides the value of the likelihood-ratio-test statistic and the corresponding p-value to 
assess whether the overdispersion observed in multiple groups of microbiome samples are equal.}
\usage{Xoc.sevsample(group.data, initscalar = 30, epsilon = 10^(-4))}

\arguments{
  \item{group.data}{
A list of matrices of data which contain samples and their taxa counts (matrices should have the same column names).}
  \item{initscalar}{
Initial value for the MLE estimation procedure, the default value is 30. See function 'dirmult' for more details.}
  \item{epsilon}{
Convergence tolerance. To terminate, the difference between two succeeding log-likelihoods must be smaller than epsilon. Default value is 10^(-4).}
}
\details{
To assess whether the over dispersion parameter vectors \eqn{\theta_{\mathrm{m}}} observed in \eqn{J} groups of microbiome samples are equal to each other, the following hypothesis 
\eqn{\mathrm{H}_{\mathrm{o}}: \theta_{\mathrm{1}} = \cdots =\theta_{\mathrm{m}} =\cdots=\theta_{\mathrm{J}}=\theta_{\mathrm{o}}} versus \eqn{\mathrm{H}_{\mathrm{a}}: \theta_{\mathrm{m}} \ne \theta_{\mathrm{o}}, m=1, \ldots, J} 
can be tested. In particular, the likelihood-ratio test statistic is used (Tvedebrink, 2010), which is given by,
\deqn{x_{\mathrm{oc}}=-2 \log\left\{\frac{L\left(\theta_{\mathrm{o}}; \mathbf{X}_{\mathrm{1}},\ldots, \mathbf{X}_{\mathrm{J}} \right)}{L\left(\theta_{\mathrm{1}},\ldots, \theta_{\mathrm{J}}; \mathbf{X}_{\mathrm{1}},\ldots, \mathbf{X}_{\mathrm{J}} \right)}\right\} .} 
The asymptotic null distribution of \eqn{x_{\mathrm{oc}}} follows a Chi-square with degrees of freedom equal to (J-1) (Wilks, 1938).
}
\value{A list containing the Xoc statistics and p-values.}
\references{
Tvedebrink, T. (2010). Overdispersion in allelic counts and theta-correction in forensic genetics. Theor Popul Biol 78, 200-210. \cr
Wilks, S. S. (1938). The Large-Sample Distribution of the Likelihood Ratio for Testing Composite Hypotheses. The Annals of Mathematical Statistics 9, 60-62.}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva) 
data(tonsils)

mygroup <- list(saliva[,1:10], tonsils[,1:10])
Xoc.sevsample_chck <- Xoc.sevsample(mygroup)
Xoc.sevsample_chck 
}