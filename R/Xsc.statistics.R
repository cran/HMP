Xsc.statistics <-
function(pi1, theta, numReads, pi0){
	totalReads <- sum(numReads)
	
	# Get Xsc value
	tempVal <- ((theta*(sum(numReads^2)-totalReads) + totalReads) / totalReads^2) * (diag(pi0)-pi0 %*% t(pi0))
	xsc <- as.vector(t(pi1-pi0) %*% MASS::ginv(tempVal) %*% (pi1-pi0))	
	
	return(xsc)
}
