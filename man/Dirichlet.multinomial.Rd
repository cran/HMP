\name{Dirichlet.multinomial}
\alias{Dirichlet.multinomial}
\title{Generation of Dirichlet-Multinomial Random Samples}
\description{Random generation of Dirichlet-Multinomial samples.}

\usage{Dirichlet.multinomial(Nrs, shape)}

\arguments{
  \item{Nrs}{A vector specifying the number of reads or sequence depth for each sample.}
  \item{shape}{A vector of Dirichlet parameters for each taxa.}
}

\value{A data matrix of taxa counts where the rows are samples and columns are the taxa.}

\details{
	The Dirichlet-Multinomial distribution is given by (Mosimann, J. E. (1962); Tvedebrink, T. (2010)),
	\deqn{\textbf{P}\left ({\textbf{X}_i}=x_{i};\left \{ \pi_j \right \},\theta\right )=\frac{N_{i}!}{x_{i1} !,\ldots,x_{iK} !}\frac{\prod_{j=1}^K \prod_{r=1}^{x_{ij}} \left \{ \pi_j \left ( 1-\theta \right )+\left ( r-1 \right )\theta\right \}}{\prod_{r=1}^{N_i}\left ( 1-\theta\right )+\left ( r-1 \right) \theta}}
	where \eqn{\textbf{x}_{i}= \left [ x_{i1}, \ldots, x_{iK} \right ]} is the random vector formed by K taxa (features) counts (RAD vector), \eqn{N_{i}= \sum_{j=1}^K x_{ij}} is the total number of reads (sequence depth), \eqn{ \left\{ \pi_j \right\}} are the mean of taxa-proportions (RAD-probability mean), and \eqn{\theta} is the overdispersion parameter.
	
	Note: Though the test statistic supports an unequal number of reads across samples, the performance has not yet been fully tested.
}

\references{
	Mosimann, J. E. (1962). On the compound multinomial distribution, the multivariate \eqn{\beta}-distribution, and correlations among proportions. Biometrika 49, 65-82.\cr 
	Tvedebrink, T. (2010). Overdispersion in allelic counts and theta-correction in forensic genetics. Theor Popul Biol 78, 200-210.
}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	
	### Generate a the number of reads per sample
	### The first number is the number of reads and the second is the number of subjects
	nrs <- rep(15000, 20) 
	
	### Get gamma from the dirichlet-multinomial parameters
	shape <- dirmult(saliva)$gamma
	
	dmData <- Dirichlet.multinomial(nrs, shape)
	dmData[1:5, 1:5]
}