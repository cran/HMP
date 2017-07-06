Xdc.statistics.MoM <-
function(group.data){	
	# Get the loglik from the fit from every data set
	logliks <- sapply(group.data, function(x){DM.MoM(x)$loglik})
	
	# Get the fit assuming all in the same group
	groupDataC <- do.call(rbind, group.data)
	groupFit <- DM.MoM(groupDataC)	
	
	# Calculate the xdc
	xdc <- -2*(groupFit$loglik-sum(logliks))
	
	return(xdc)
}
