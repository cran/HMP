\name{Plot.PI}
\alias{Plot.PI}
\title{Plot the Pi Vector}
\description{Plots the taxa proportions for every group.}

\usage{
	Plot.PI(estPi, errorBars = TRUE, logScale = FALSE, 
		main = "PI Vector", ylab = "Fractional Abundance")
}

\arguments{
  \item{estPi}{The results for either MLE or MOM from the function 'Est.Pi'.}
  \item{errorBars}{A boolean to display the error bars or not.}
  \item{logScale}{A boolean to log the y scale or not.}
  \item{main}{A string to be used as the plots title.}
  \item{ylab}{A string to be used as the plots y-axis title.}
}

\value{A plot of the pi vectors for every group.}

\examples{
	\dontrun{
		data(saliva)
		data(throat)
		data(tonsils)
		
		### Combine the data sets into a single list
		group.data <- list(saliva, throat, tonsils)
		
		### Get PI using MLE with CI
		mle <- Est.PI(group.data)$MLE
		
		### Plot with Error Bars
		Plot.PI(mle)
		
		### Plot without Error Bars
		Plot.PI(mle, FALSE)
		
		### Plot with Error Bars and scaling
		Plot.PI(mle, TRUE, TRUE)
	}
}