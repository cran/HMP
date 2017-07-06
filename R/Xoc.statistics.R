Xoc.statistics <-
function(group.data, epsilon=10^(-4)){
	numGroups <- length(group.data)
	
	# Get the fit for every data set
	thetas <- rep(0, numGroups)
	logliks <- rep(0, numGroups)
	pis <- vector("list", numGroups)
	for(i in 1:numGroups){
		tempTheta <- DM.MoM(group.data[[i]])$theta
		fit <- dirmult::dirmult(group.data[[i]], initscalar=(1-tempTheta)/tempTheta, epsilon=epsilon, trace=FALSE)
		
		thetas[i] <- fit$theta
		logliks[i] <- fit$loglik
		pis[[i]] <- fit$pi
	}
	
	# Get the fit assuming equal thetas
	equalFit <- dirmult::equalTheta(group.data, mean(thetas), epsilon, FALSE, pis)
	
	# Calculate the xoc
	xoc <- as.vector(-2*(equalFit$loglik-sum(logliks)))
	
	return(xoc)
}
