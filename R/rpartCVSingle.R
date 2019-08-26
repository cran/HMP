rpartCVSingle <-
function(data, covars, cvNum, cpTable, dropGrps, numCPLvls, minsplit, minbucket){
	# Get location of data to drop
	dropLoc <- which(dropGrps == cvNum)
	
	# Seperate dropped data
	subData <- data[-dropLoc,, drop=FALSE]
	dropData <- data[dropLoc,, drop=FALSE]
	
	# Seperate dropped covars
	subCovs <- covars[-dropLoc,, drop=FALSE]
	dropCovs <- covars[dropLoc,, drop=FALSE]
	
	# Run rpart on smaller data
	subRpartRes <- DM.Rpart.Base(subData, subCovs, FALSE, minsplit, minbucket)$fullTree
	
	# Need to be abandance for later code
	subData <- subData/(rowSums(subData))
	dropData <- dropData/(rowSums(dropData))
	
	# Calculate relative error
	MSEn <- rep(NA, numCPLvls)
	subTree <- vector("list", numCPLvls)
	for(i in 1:numCPLvls){
		subTree[[i]] <-  rpart::prune(subRpartRes, cp=cpTable[i, 1])
		pruneSubTree <- subTree[[i]]
		subPre <- predict(pruneSubTree, newdata=subCovs, type="vector")
		dropPre <- predict(pruneSubTree, newdata=dropCovs, type="vector")
		
		## 1.a. Distance: new subject to the mean Taxa in the signed Terminal node
		tempDist <- 0
		for(j in 1:length(dropPre))
			tempDist <- tempDist + (dist(rbind(dropData[j,], colMeans(subData[subPre == dropPre[j],]))))^2
		
		MSEn[i] <- tempDist/length(dropPre)
	}
	names(MSEn) <- cpTable[,2] + 1
	
	return(list(errorRate=MSEn, subTree=subTree))
}
