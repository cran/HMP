Xoc.sevsample <-
function(group.data, epsilon=10^(-4)){
	if(missing(group.data))
		stop("group.data missing.")
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
	}
	
	numGroups <- length(group.data)
	
	# Get Xoc and calculate pvalue
	Xoc <- Xoc.statistics(group.data, epsilon)
	pval <- 1-pchisq(q=Xoc, df=numGroups-1, ncp=0, lower.tail=TRUE)	
	
	sev.overd.test <- list("Xoc statistics"=Xoc, "p value"=pval)
	
	return(sev.overd.test)			
}
