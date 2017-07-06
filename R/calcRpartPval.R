calcRpartPval <-
function(rawResults, rpartPermRes, numPerms){
	# Approx values for permuted trees based on our real tree
	treeNames <- levels(rpartPermRes$Tree)
	pvalData <- matrix(NA, nrow(rawResults), numPerms)
	colnames(pvalData) <- treeNames
	for(i in 1:numPerms){
		id <- which(rpartPermRes$Tree == treeNames[i])
		if(length(id) <= 1) # Skip any 1 node trees
			next
		pvalData[,i] <- stats::approx(rpartPermRes$Leafs[id], rpartPermRes$RelErr[id], rawResults$Leafs)$y
	}
	
	# Calculate P-value
	pval <- rowMeans(rawResults$RelErr >= pvalData, na.rm=TRUE)
	pval <- ifelse(is.na(pval), 0, pval)
	
	return(pval)
}
