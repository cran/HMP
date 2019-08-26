formatDataSets <-
function(group.data){
	if(missing(group.data))
		stop("group.data missing.")
	
	# Make sure we have more than 1 data set
	numGroups <- length(group.data)
	if(numGroups < 2)
		stop("At least 2 data sets are required.")
	
	# Merge all the data together
	dataNames <- vector("list", numGroups)
	newData <- NULL
	for(i in 1:length(group.data)){		
		tempData <- group.data[[i]]
		
		# Remove any all 0 subjects from the data
		tempData <- tempData[rowSums(tempData) != 0,, drop=FALSE]
		
		# Save the current row names
		dataNames[[i]] <- rownames(tempData)
		
		newData <- merge(newData, t(group.data[[i]]), by=0, all=TRUE)
		rownames(newData) <- newData[,1]
		newData <- newData[,-1]
	}
	
	# Remove any nas
	newData[is.na(newData)] <- 0
	newData <- t(newData)
	
	# Remove any all 0 columns and sort them
	newData <- newData[,colSums(newData) != 0, drop=FALSE]
	newData <- newData[,order(colSums(newData), decreasing=TRUE)]
	
	# Turn the data back into a list
	retData <- vector("list", numGroups)
	base <- 0
	for(i in 1:numGroups){
		retData[[i]] <- newData[(base+1):(nrow(group.data[[i]])+ base),]
		rownames(retData[[i]]) <- dataNames[[i]]
		
		base <- base + nrow(group.data[[i]])
	}
	
	names(retData) <- names(group.data)
	return(retData)
}
