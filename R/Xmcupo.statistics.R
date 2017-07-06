Xmcupo.statistics <-
function(groupParameter){	
	numGroups <- length(groupParameter)	
	numTaxa <- length(groupParameter[[1]]$pi)
	
	# Pull out pi and calculate xsc
	pis <- matrix(0, numTaxa, numGroups)
	xscs <- rep(0, numGroups)
	for(i in 1:numGroups){
		theta <- groupParameter[[i]]$theta
		numReads <- groupParameter[[i]]$nrs
		totalReads <- sum(numReads)
		
		# Calculate the Xsc for each group
		xscs[i] <- (theta * (sum(numReads^2)-totalReads) + totalReads) / totalReads^2
		pis[,i] <- groupParameter[[i]]$pi
	}
	
	# Remove any 0 taxa
	pis <- pis[rowSums(pis)!=0,]
	
	# Calculate pi0
	pi0 <- rowSums(pis/xscs)/sum(1/xscs)
	
	# Calculate Xmcupo
	Xmcupo <- sum(colSums((pis-pi0)^2/pi0)/xscs)		
	
	return(Xmcupo)
}
