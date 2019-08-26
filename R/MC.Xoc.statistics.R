MC.Xoc.statistics <-
function(group.Nrs, numMC=10, group.alphap, type="ha", siglev=0.05) {
	if(missing(group.alphap) || missing(group.Nrs))
		stop("group.Nrs and/or group.alphap missing.")
	if(tolower(type) != "ha" && tolower(type) != "hnull")
		stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
	
	numGroups <- length(group.Nrs)	
	
	# Get all the Xoc values
	XocStatVector <- rep(0, numMC)
	for(i in 1:numMC)
		XocStatVector[i] <- Xoc.statistics.Hnull.Ha(group.Nrs, group.alphap, type)
	
	# Get a reference value from the real data
	qAlpha <- qchisq(p=(1-siglev), df=(numGroups-1), ncp=0, lower.tail=TRUE)
	
	# Calculate pvalues
	pval <- (sum(XocStatVector > qAlpha) + 1)/(length(XocStatVector) + 1)
	
	return(pval)
}
