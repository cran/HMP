Xmcupo.effectsize <-
function(group.data){
	if(missing(group.data))
		stop("group.data missing.")
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
		numTaxa <- ncol(group.data[[1]])
	}
	
	numGroups <- length(group.data)
	totalReads <- sum(sapply(group.data, sum))
	
	if(numTaxa < numGroups)
		stop("The number of taxa must be greater than the number of groups.")
	
	# Get the parameters for every group
	groupParameter <- lapply(group.data, function(x){
				# Calc pi, theta and the number of reads
				numReadsSubs <- rowSums(x)
				pi.MoM <- colSums(x)/sum(x)
				theta.MoM <- weirMoM(x, pi.MoM)$theta
				
				return(list(pi=pi.MoM, theta=theta.MoM, nrs=numReadsSubs))
			})
	
	# Calculate Xmcupo stats for base data
	Xmcupo <- Xmcupo.statistics(groupParameter)
	
	# Edit parameters to use the biggest difference between pis
	groupParameterMax <- groupParameter
	for(i in 1:numGroups){
		newPi <- rep(0, numTaxa)
		newPi[i] <- 1
		groupParameterMax[[i]]$pi <- newPi
	}
	
	# Calculate Xmcupo stats for biggest difference
	XmcupoMax <- Xmcupo.statistics(groupParameterMax)
	
	# Calculate Cramers
	CramerV	<- sqrt(Xmcupo/(totalReads*min(numTaxa-1, numGroups-1)))
	Mod.CramerV <- sqrt(Xmcupo/XmcupoMax)
	
	# Calculate pvalue
	pval <- 1-pchisq(q=Xmcupo, df=(numGroups-1)*(numTaxa-1), ncp=0, lower.tail=TRUE)
	
	result 	<- c("Chi-Squared"=Xmcupo, "Cramer Phi"=CramerV, "Modified-Cramer Phi"=Mod.CramerV, "P value"=pval)
	return(result)
}
