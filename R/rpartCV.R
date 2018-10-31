rpartCV <-
function(data, covars, rpartRes, minsplit, minbucket, numCV){
	# Pull out cp info
	cpTable <- rpartRes$cptable
	numCPLvls <- nrow(cpTable)
	
	# New cp for pruning
	if(numCPLvls > 2){
		oldCPs <- cpTable[,1]
		
		cpTable[1, 1] <- Inf
		cpTable[numCPLvls, 1] <- 0
		for(m in 2:(numCPLvls-1))
			cpTable[m, 1] <- sqrt(oldCPs[m] * oldCPs[m-1])
	}
	
	# Set up groups for dropping data
	numSub <- nrow(data)
	dropNums <- cut(1:numSub, numCV, FALSE)
	dropGrps <- sample(dropNums, numSub)
	
	errorRate <- vector("list", numCV)
	subTree <- vector("list", numCV)
	for(k in 1:numCV){
		# Get location of data to drop
		dropLoc <- which(dropGrps == k)
		
		# Seperate dropped data
		subData <- data[-dropLoc,, drop=FALSE]
		dropData <- data[dropLoc,, drop=FALSE]
		
		# Seperate dropped covars
		subCovs <- covars[-dropLoc,, drop=FALSE]
		dropCovs <- covars[dropLoc,, drop=FALSE]
		
		# Run rpart on smaller data
		subRpartBase <- DM.Rpart.Base(subData, subCovs, FALSE, minsplit, minbucket)
		subRpartRes <- subRpartBase$fullTree
		
		# Need to be abandance for later code
		subData <- subData/(rowSums(subData))
		dropData <- dropData/(rowSums(dropData))
		
		# Calculate relative error
		dist <- NULL
		subTree[[k]] <- vector("list", numCPLvls)
		for(i in 1:numCPLvls){
			subTree[[k]][[i]] <-  rpart::prune(subRpartRes, cp=cpTable[i, 1])
			pruneSubTree <- subTree[[k]][[i]]
			subPre <- predict(pruneSubTree, newdata=subCovs, type="vector")
			dropPre <- predict(pruneSubTree, newdata=dropCovs, type="vector")
			
			## 1.a. Distance: new subject to the mean Taxa in the signed Terminal node
			tempDist <- 0
			for(j in 1:length(dropPre))
				tempDist <- tempDist + (dist(rbind(dropData[j,], colMeans(subData[subPre == dropPre[j],]))))^2
			
			dist[i] <- tempDist/length(dropPre)
		}
		
		error <- data.frame(DtoM=dist)
		rownames(error) <- cpTable[,2] + 1
		errorRate[[k]] <- error
	}
	
	# Calculate the square root of the errors
	erSqrt <- lapply(errorRate, sqrt)
	error <- do.call("cbind", erSqrt)
	
	# Pull out only the distance to the mean
	loc <- which(colnames(error) == "DtoM")
	errorDM <- as.matrix(error[,loc])
	
	# Calculate CI of DtoM
	ciInfo <- matrix(NA, numCPLvls, 4)
	for(j in 1:numCPLvls){
		ciInfo[j, 1] <- mean(errorDM[j,])
		ciInfo[j, 2:3] <- rpartCI(errorDM[j,], 0.95)
		ciInfo[j, 4] <- sd(errorDM[j,])/sqrt(ncol(errorDM))
	}
	
	ciInfo <- cbind(ciInfo, rank(ciInfo[,1]))
	colnames(ciInfo) <- c("DtoM", "SE", "Lower", "Upper", "Rank")
	
	# Add ci info back into cp table
	cpTable2 <- cbind(rpartRes$cptable, ciInfo)
	
	return(list(subTree=subTree, errorRate=errorRate, ciInfo=cpTable2))
}
