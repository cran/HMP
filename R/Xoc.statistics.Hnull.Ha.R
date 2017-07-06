Xoc.statistics.Hnull.Ha <-
function(group.Nrs, group.alphap, type){
	numGroups <- length(group.Nrs)
	tempShape <- group.alphap
	
	# Generate a new set of data
	genGroupData <- vector("list", numGroups)
	for(i in 1:numGroups){
		if(tolower(type) == "ha")
			tempShape <- group.alphap[i,]
		
		genGroupData[[i]] <- Dirichlet.multinomial(group.Nrs[[i]], tempShape)
	}
	
	# Get the xoc stats for the generated data
	xoc <- Xoc.statistics(genGroupData)	
	
	return(xoc)
}
