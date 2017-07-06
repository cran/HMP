Xmcupo.sevsample <-
function(group.data){
	if(missing(group.data))
		stop("group.data is missing.")
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
		numTaxa <- ncol(group.data[[1]])
	}
	
	numGroups <- length(group.data)
	
	# Get the parameters for every group
	groupParameter <- lapply(group.data, function(x){
				# Calc pi, theta and the number of reads
				numReadsSubs <- rowSums(x)
				pi.MoM <- colSums(x)/sum(x)
				theta.MoM <- weirMoM(x, pi.MoM)$theta
				
				return(list(pi=pi.MoM, theta=theta.MoM, nrs=numReadsSubs))
			})
	
	# Get Xmcupo and calculate pvalue
	Xmcupo <- Xmcupo.statistics(groupParameter)
	pval <- 1-pchisq(q=Xmcupo, df=(numGroups-1)*(numTaxa-1), ncp=0, lower.tail=TRUE)
	
	ret <- list("Xmcupo statistics"=Xmcupo, "p value"=pval)
	
	return(ret)	
}
