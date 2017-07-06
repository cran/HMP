Xdc.statistics <-
function(group.data, epsilon=10^(-4)){ 
	# Get the loglik from the fit from every data set
	logliks <- sapply(group.data, function(x, epsilon){
				tempTheta <- DM.MoM(x)$theta
				dirmult::dirmult(x, initscalar=(1-tempTheta)/tempTheta, epsilon=epsilon, trace=FALSE)$loglik
			}, epsilon=epsilon)
	
	# Get the fit assuming all in the same group
	groupDataC <- do.call(rbind, group.data)
	tempTheta <- DM.MoM(groupDataC)$theta
	groupFit <- dirmult::dirmult(groupDataC, initscalar=(1-tempTheta)/tempTheta, epsilon=epsilon, trace=FALSE)	
	
	# Calculate the xdc
	xdc <- -2*(groupFit$loglik-sum(logliks))
	
	return(xdc)
}
