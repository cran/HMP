Xsc.onesample <-
function(data, pi0){
	if(missing(data) || missing(pi0))
		stop("data and/or pi0 missing.")
	
	numReadsSubs <- rowSums(data)
	numTaxa	<- length(pi0)
	
	# Check the data set has the same number of taxa
	numTaxa	<- length(pi0)
	if(ncol(data) != numTaxa)
		stop("Every data set must have the same length as pi0")
	
	# Get parameters
	fit.MoM <- DM.MoM(data)
	
	# Get Xsc and calculate pvalue
	Xsc <- Xsc.statistics(fit.MoM$pi, fit.MoM$theta, numReadsSubs, pi0)
	pval <- 1-pchisq(q=Xsc, df=numTaxa-1, ncp=0, lower.tail=TRUE)
	
	RAD.mean.test <- list("Xsc statistics"=Xsc, "p value"=pval)
	
	return(RAD.mean.test)			
}
