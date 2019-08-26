MC.Xdc.statistics <-
function(group.Nrs, numMC=10, alphap, type="ha", siglev=0.05, est="mom") {
	if(missing(alphap) || missing(group.Nrs))
		stop("group.Nrs and/or alphap  missing.")
	if(tolower(type) != "ha" && tolower(type) != "hnull")
		stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
	if(tolower(est) != "mom" && tolower(est) != "mle")
		stop(sprintf("Est '%s' not found. Est must be 'mle' or 'mom'.", as.character(est)))
	
	numGroups <- length(group.Nrs)	
	
	if(tolower(type) == "hnull"){
		numTaxa <- length(alphap)
	}else{
		numTaxa <- ncol(alphap)
	}
	
	# Get all the Xdc values
	XdcStatVector <- rep(0, numMC)
	for(i in 1:numMC)
		XdcStatVector[i] <- Xdc.statistics.Hnull.Ha(alphap, group.Nrs, type, est)
	
	# Get a reference value from the real data
	qAlpha <- qchisq(p=(1-siglev), df=(numGroups-1)*numTaxa, ncp=0, lower.tail=TRUE)
	
	# Calculate pvalues
	pval <- (sum(XdcStatVector > qAlpha) + 1)/(length(XdcStatVector) + 1)
	
	return(pval)
}
