MC.Xmc.statistics <-
function(group.Nrs, numMC=10, pi0, group.pi, group.theta, type="ha", siglev=0.05, MC=NULL, Nrs=NULL) {
	# Check if someone is still using Nrs
	if(!is.null(Nrs)){
		warning("'Nrs' is deprecated. It has been replaced with group.Nrs. View the help files for details.")
		group.Nrs <- Nrs
	}
	
	if(missing(group.theta) || missing(pi0) || missing(group.Nrs))
		stop("group.Nrs, pi0 and/or group.theta missing.")
	if(missing(group.pi) && tolower(type) == "ha")
		stop("group.pi missing.")
	if(tolower(type) != "ha" && tolower(type) != "hnull")
		stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
	
	# Check if someone is still using MC
	if(!is.null(MC)){
		warning("'MC' is deprecated. It has been replaced with numMC. View the help files for details.")
		numMC <- MC
	}
	
	numGroups <- length(group.Nrs)
	numTaxa <- length(pi0)
	
	# If the type is ha this will change in the for loop
	tempPi <- pi0
	
	# Create the parameters for every group
	groupParameter <- vector("list", numGroups)
	for (i in 1:numGroups){
		if(tolower(type) == "ha")
			tempPi <- group.pi[i,]
		groupParameter[[i]] <- list(pi=tempPi, theta=group.theta[i], nrs=group.Nrs[[i]])
	}
	
	# Get all the Xmc values
	XmcStatVector <- rep(0, numMC)
	for(i in 1:numMC)
		XmcStatVector[i] <- Xmc.statistics.Hnull.Ha(groupParameter, pi0)
	
	# Get a reference value from the real data
	qAlpha <- qchisq(p=(1-siglev), df=length(group.theta)*(numTaxa-1), ncp=0, lower.tail=TRUE)
	
	# Calculate pvalues
	pval <- (sum(XmcStatVector > qAlpha) + 1)/(length(XmcStatVector) + 1)
	
	return(pval)
}
