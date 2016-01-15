\name{C.alpha.multinomial}
\alias{C.alpha.multinomial}
\title{\eqn{C(\alpha)} - Optimal Test for Assessing Multinomial Goodness of Fit Versus Dirichlet-Multinomial Alternative}
\description{
	A function to compute the \eqn{C(\alpha)}-optimal test statistics of Kim and Margolin (1992) 
	for evaluating the Goodness-of-Fit of a Multinomial distribution (null hypothesis) versus a Dirichlet-Multinomial 
	distribution (alternative hypothesis).
}

\usage{C.alpha.multinomial(data)}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
}

\value{A list containing the \eqn{C(\alpha)}-optimal test statistic and p-value.}

\details{
	In order to test if a set of ranked-abundance distribution(RAD) from microbiome samples can be modeled better using a multinomial or Dirichlet-Multinomial 
	distribution, we test the hypothesis \eqn{\mathrm{H}: \theta = 0} versus  \eqn{\mathrm{H}: \theta \ne 0}, 
	where the null hypothesis  implies a multinomial distribution and the alternative hypothesis  implies a DM distribution. 
	Kim and Margolin (Kim and Margolin, 1992) proposed a \eqn{C(\alpha)}-optimal test- statistics given by,
	
	\deqn{T = \sum_{j=1}^{K} \sum_{i=1}^{P} \frac{1}{\sum_{i=1}^{P} x_{ij}}\left (x_{ij}-\frac{N_{i}\sum_{i=1}^{P} x_{ij}}{N_{\mathrm{g}}} \right )^2} 
	
	Where \eqn{K} is the number of taxa, \eqn{P} is the number of samples, \eqn{x_{ij}} is the taxon \eqn{j}, \eqn{j = 1,\ldots,K} from sample \eqn{i}, 
	\eqn{i=1,\ldots,P}, \eqn{N_{i}} is the number of reads in sample \eqn{i}, and   \eqn{N_{\mathrm{g}}} is the total number of reads across samples. 
	
	As the number of reads increases, the distribution of the \eqn{T} statistic converges to a Chi-square with degrees of freedom 
	equal to \eqn{(P-1)(K-1)}, when the number of sequence reads is the same in all samples. When the number of reads is not the same in all samples, 
	the distribution becomes a weighted Chi-square with a modified degree of freedom (see (Kim and Margolin, 1992) for more details).
	
	Note: Each taxa in \code{data} should be present in at least 1 sample, a column with all 0's may result in errors and/or invalid results.
}

\references{
	Kim, B. S., and Margolin, B. H. (1992). Testing Goodness of Fit of a Multinomial Model Against Overdispersed Alternatives. Biometrics 48, 711-719.
}

\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
	data(saliva)
	
	calpha_check <- C.alpha.multinomial(saliva)
	calpha_check
}