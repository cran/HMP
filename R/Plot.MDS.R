Plot.MDS <-
function(group.data, main="Group MDS", retCords=FALSE){
	if(missing(group.data))
		stop("group.data is missing.")
	
	numGroups <- length(group.data)
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
	}
	
	# Make sure we have group names
	if(is.null(names(group.data))){
		grpNames <- paste("Data Set", 1:numGroups)
	}else{
		grpNames <- names(group.data)
	}
	
	# Merge all the data sets together
	mData <- do.call("rbind", group.data)
	
	# Get their mds location 
	loc <- getBC(mData)
	
	# Set color
	availCols <- rainbow(numGroups)
	cols <- NULL
	for(i in 1:numGroups)
		cols <- c(cols, rep(availCols[i], nrow(group.data[[i]])))
	
	# Plot MDS
	plot(loc, pch=16, ylab="MDS 2", xlab="MDS 1", col=cols)
	legend("topright", legend=grpNames, pch=16, col=availCols)
	
	if(retCords)
		return(loc)
}
