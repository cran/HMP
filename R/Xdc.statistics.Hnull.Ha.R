Xdc.statistics.Hnull.Ha <-
function(alphap, group.Nrs, type, est){
	numGroups <- length(group.Nrs)
	tempShape <- alphap
	
	# Generate a new set of data
	genGroupData <- vector("list", numGroups)
	for(i in 1:numGroups){
		if(tolower(type) == "ha")
			tempShape <- alphap[i,]
		
		genGroupData[[i]] <- Dirichlet.multinomial(group.Nrs[[i]], tempShape)
	}
	
	# Get the xdc stats for the generated data
	if(tolower(est) == "mle"){
		xdc <- Xdc.statistics(genGroupData)
	}else{
		xdc <- Xdc.statistics.MoM(genGroupData)
	}
	
	return(xdc)
}
