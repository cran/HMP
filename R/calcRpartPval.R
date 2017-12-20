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
		temp <- stats::approx(rpartPermRes$Leafs[id], rpartPermRes$WDist[id], rawResults$Leafs)$y
#		pvalData[,i] <- -c(0, diff(temp)/diff(rawResults$Leafs))
		pvalData[,i] <- temp
	}
	
	# Calculate P-value
#	slope <- -c(0, diff(rawResults$WDist)/diff(rawResults$Leafs))
#	pval <- rowMeans(slope <= pvalData, na.rm=TRUE)
	pval <- rowMeans(rawResults$WDist> pvalData, na.rm=TRUE)
	pval <- ifelse(is.na(pval), 0, pval)
	
	return(pval)
}
