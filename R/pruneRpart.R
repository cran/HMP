pruneRpart <-
function(rpartResults, rpartData, iter){
	# Turn data into abundance
#	abunData <- t(apply(rpartData, 1, function(x){x/sum(x)}))
	
	# Pull out cp values for nodes
	cp <- rpartResults$cp[,1]
	relErr <- rpartResults$cp[,3]
	
	# Calculate within group distance and # of terminal nodes        
	wDist <- rep(0, length(cp))
	numLeafs <- rep(0, length(cp))
	
	# Run permuted data at every cp level
	for(i in 1:length(cp)){
		resTemp <- rpart::prune(rpartResults, cp[i] + 10^(-10)) # We need to add a tiny amount to cp due to rounding issues
		
		# Find all the leaf nodes
		leafSplits <- unique(resTemp$where)
		numLeafs[i] <- length(leafSplits)
		
		# Calc distance within each leaf
#		for(j in 1:numLeafs[i]){
#			leafId <- which(resTemp$where == leafSplits[j])
#			if(length(leafId) == 1)
#				next
#			wDist[i] <- wDist[i] + sum(dist(abunData[leafId,]))
#		}
	}
	tempResults <- data.frame(Tree=paste("Tree", iter), CP=cp, Leafs=numLeafs, WDist=wDist, RelErr=relErr)
	
	return(tempResults)
}
