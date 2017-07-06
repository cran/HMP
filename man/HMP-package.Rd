\name{HMP-package}
\alias{HMP-package}
\alias{HMP}
\docType{package}
\title{Hypothesis Testing and Power Calculations for Comparing Metagenomics Samples}

\description{
	This package provides tools for Generating data matrices following Multinomial and Dirichlet-Multinomial distributions, 
	Computing the following test-statistics and their corresponding p-values, and Computing the power and size 
	of the tests described above using Monte-Carlo simulations.
}

\details{
	\tabular{lll}{
		\eqn{\textbf{Hypothesis Test}}	\tab \eqn{\textbf{Test Statistics Function}}	\tab \eqn{\textbf{Power Calculation Function}}	\cr
		2+ Sample Means w/ Reference Vector	\tab \code{Xmc.sevsample}	\tab \code{MC.Xmc.statistics} \cr
		1 Sample Mean w/ Reference Vector	\tab \code{Xsc.onesample}	\tab \code{MC.Xsc.statistics} \cr
		2+ Sample Means w/o Reference Vector	\tab \code{Xmcupo.sevsample}	\tab \code{MC.Xmcupo.statistics} \cr
		2+ Sample Overdispersions	\tab \code{Xoc.sevsample}	\tab \code{MC.Xoc.statistics} \cr
		2+ Sample DM-Distribution	\tab \code{Xdc.sevsample}	\tab \code{MC.Xdc.statistics}	\cr
		Multinomial vs DM	\tab \code{C.alpha.multinomial}	\tab \code{MC.ZT.statistics} \cr
	}
	
	In addition to hypothesis testing and power calculations you can: \cr
	\enumerate{
		\item Perform basic data management to exclude samples with fewer than pre-specified number of reads, 
			collapse rare taxa and order the taxa by frequency. This is useful to exclude failed samples 
			(i.e. samples with very few reads) - \code{Data.filter}
		\item Plot your data - \code{Barchart.data} 
			\item Generate random sample of Dirichlet Multinomial data with pre-specified parameters - \code{Dirichlet.multinomial}
	}	
	
	Note: Thought the description of the functions refer its application to ranked abundance distributions (RAD) data, 
	every function is also applicable to model species abundance data. See references for a discussion and application to both 
	type of ecological data.
}

\references{
	La Rosa PS, Brooks JP, Deych E, Boone EL, Edwards DJ, et al. (2012) Hypothesis Testing and Power Calculations for 
	Taxonomic-Based Human Microbiome Data. PLoS ONE 7(12): e52078. doi:10.1371/journal.pone.0052078
}

\author{
	Patricio S. La Rosa, Elena Deych, Berkley Shands, Sharina Carter, William D. Shannon
}

\keyword{ package }