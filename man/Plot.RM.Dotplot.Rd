\name{Plot.RM.Dotplot}
\alias{Plot.RM.Dotplot}
\title{Plot the Pi Vector for Repeated Measures}
\description{Plots the taxa proportions for every group/time as a dot plot.}

\usage{
	Plot.RM.Dotplot(group.data, groups, times, errorBars = TRUE, 
		col = NULL, conf = .95, alpha = 1)
}

\arguments{
  \item{group.data}{A list of matrices of taxonomic counts(columns) for each sample(rows).}
  \item{groups}{A vector indicating group membership.}
  \item{times}{A vector indicating time.}
  \item{errorBars}{When 'TRUE', error bars will also be displayed.}
  \item{col}{A vector of colors to use to denote taxa.}
  \item{conf}{The desired confidence limits. The default is 95\%}
  \item{alpha}{The desired alpha level for the colors.}
}

\value{A plot of the pi vectors for every group/time.}

\author{Elena Deych, Berkley Shands, William D. Shannon, Dake Yang}

\examples{
	\dontrun{
		data(saliva)
		data(throat)
		
		### Reduce the size of the data
		saliva <- Data.filter(saliva, numTaxa=9)
		throat <- Data.filter(throat, numTaxa=9)
		
		### Get the gamma value for the data
		saliva.gamma <- DM.MoM(saliva)$gamma
		throat.gamma <- DM.MoM(throat)$gamma
		mid.gamma <- (saliva.gamma + throat.gamma)/2
		
		### Generate a the number of reads per sample
		### The first number is the number of reads and the second is the number of subjects
		nrs <- rep(10000, 20)
		
		### Create data sets to be our time series in a list
		group.data <- list(
				Dirichlet.multinomial(nrs, saliva.gamma),
				Dirichlet.multinomial(nrs, saliva.gamma),
				Dirichlet.multinomial(nrs, saliva.gamma),
				Dirichlet.multinomial(nrs, saliva.gamma),
				Dirichlet.multinomial(nrs, mid.gamma),
				Dirichlet.multinomial(nrs, throat.gamma)
		)
		names(group.data) <- c(
				"Group 1, Time 1", "Group 2, Time 1",
				"Group 1, Time 2", "Group 2, Time 2",
				"Group 1, Time 3", "Group 2, Time 3"
		)
		
		### Set the group and time information for each list element
		groups <- c(1, 2, 1, 2, 1, 2)
		times <- c(1, 2, 3, 1, 2, 3)
		
		### Plot the data with error bars
		Plot.RM.Dotplot(group.data, groups, times)
		
		### Plot the data without error bars
		Plot.RM.Dotplot(group.data, groups, times, FALSE)
	}
}