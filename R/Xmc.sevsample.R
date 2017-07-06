Xmc.sevsample <-
function(group.data, pi0){	
	if(missing(group.data) || missing(pi0))
		stop("group.data and/or pi0 missing.")
	
	# Check every data set has the same number of taxa
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- length(pi0)
	if(any(taxaCounts != numTaxa))
		stop("Every data set must have matching taxa, including pi0")
	
	numGroups <- length(group.data)
	
	# Get the parameters for every group
	groupParameter <- lapply(group.data, function(x){
				# Calc pi, theta and the number of reads
				numReadsSubs <- rowSums(x)
				pi.MoM <- colSums(x)/sum(x)
				theta.MoM <- weirMoM(x, pi.MoM)$theta
				
				return(list(pi=pi.MoM, theta=theta.MoM, nrs=numReadsSubs))
			})
	
	# Get Xmc and calculate pvalue
	Xmc <- Xmc.statistics(groupParameter, pi0)
	pval <- 1-pchisq(q=Xmc, df=numGroups*(numTaxa-1), ncp=0, lower.tail=TRUE)
	
	sevRAD.mean.test <- list("Xmc statistics"=Xmc, "p value"=pval)
	
	return(sevRAD.mean.test)					
}
