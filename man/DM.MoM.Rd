\name{DM.MoM}
\alias{DM.MoM}
\title{Method-of-Moments (MoM) Estimators of the Dirichlet-Multinomial Parameters}
\description{Method-of-Moments (MoM) estimators of the Dirichlet-multinomial parameters: taxa proportions and overdispersion.}

\usage{DM.MoM(data)}

\arguments{
  \item{data}{A matrix of taxonomic counts(columns) for each sample(rows).}
}

\value{
	A list providing the MoM estimator for overdispersion, the MoM estimator of the RAD-probability mean vector, 
	and the corresponding loglikelihood value for the given data set and estimated parameters.
}

\details{
	Given a set of taxa-count vectors \eqn{\left\{\textbf{x}_{i},\ldots, \textbf{x}_{P} \right\}},  the methods of moments (MoM) estimator of the set of parameters \eqn{\theta} and  \eqn{\left\{\pi_{j}  \right\}_{j=1}^K } is given as follows (Mosimann, 1962; Tvedebrink, 2010): 
	\deqn{\hat{\pi}_{j}=\frac{\sum_{i=1}^P x_{ij}}{\sum_{i=1}^P N_{i}},} and 
	\deqn{\hat{\theta} = \sum_{j=1}^K \frac{S_{j}-G_{j}}{\sum_{j=1}^{K}\left ( S_{j}+\left ( N_{c}-1 \right )G_{j} \right )},}
	where \eqn{N_{c}=\left( P -1 \right)^{-1} \left(\sum_{i=1}^P N_{i}-\left (\sum_{i=1}^P N_{i}  \right )^{-1} \sum_{i=1}^P N_{i}^2\right)}, 
	and \eqn{S_{j}=\left( P -1 \right)^{-1} \sum_{i=1}^P N_{i} \left ( \hat{\pi}_{ij} -\hat{\pi}_{j} \right )^{2}}, 
	and \eqn{G_{j}=\left( \sum_{i=1}^P \left (N_i-1 \right ) \right)^{-1} \sum_{i=1}^P N_{i} \hat{\pi}_{ij} \left (1- \hat{\pi}_{ij}\right)} with \eqn{\hat{\pi}_{ij}=\frac{x_{ij}}{N_{i}}}.
}

\references{
	Mosimann, J. E. (1962). On the compound multinomial distribution, the multivariate \eqn{\beta}-distribution, and correlations among proportions. Biometrika 49, 65-82. \cr
	Tvedebrink, T. (2010). Overdispersion in allelic counts and theta-correction in forensic genetics. Theor Popul Biol 78, 200-210.
}

\examples{
	data(throat)
	
	fit.throat <- DM.MoM(throat)
	fit.throat
}