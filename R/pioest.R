pioest <-
function(group.data){
	if(missing(group.data))
		stop("data.groups missing.")
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
		numTaxa <- ncol(group.data[[1]])
	}
	
	numGroups <- length(group.data)
	
	# Pull out pi and calculate xsc
	pis <- matrix(0, numTaxa, numGroups)
	xscs <- rep(0, numGroups)
	for(i in 1:numGroups){
		tempData <- group.data[[i]]
		
		numReadsSubs <- rowSums(tempData)
		totalReads <- sum(tempData)
		pi <- colSums(tempData)/totalReads
		theta <- weirMoM(tempData, pi)$theta
		
		xscs[i] <- (theta * (sum(numReadsSubs^2)-totalReads) + totalReads) / totalReads^2
		pis[,i] <- pi
	}
	
	# Remove any 0 taxa
	pis <- pis[rowSums(pis) != 0,]
	
	# Calculate pi0
	pi0 <- rowSums(pis/xscs)/sum(1/xscs)
	
	names(pi0) <- colnames(group.data[[1]])
	
	return(pi0)
}
