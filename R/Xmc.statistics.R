Xmc.statistics <-
function(groupParameter, pi0){
	numGroups <- length(groupParameter)	
	numTaxa <- length(pi0)
	
	xsc <- rep(0, numGroups)
	for(i in 1:numGroups){
		pi <- groupParameter[[i]]$pi
		theta <- groupParameter[[i]]$theta
		numReads <- groupParameter[[i]]$nrs
		
		# Get Xsc values
		xsc[i] <- Xsc.statistics(pi, theta, numReads, pi0)
	}
	xmc <- sum(xsc)
	
	return(xmc)
}
