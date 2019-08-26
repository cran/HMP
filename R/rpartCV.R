rpartCV <-
function(data, covars, rpartRes, minsplit, minbucket, numCV, parallel, cores){
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
	if(parallel){
		cl <- parallel::makeCluster(min(cores, numCV)) 
		doParallel::registerDoParallel(cl)
		tryCatch({
					cvRes <- foreach::foreach(k=1:numCV, .combine=append, .multicombine=FALSE, .inorder=FALSE, .errorhandling="pass", .packages=c("rpart", "HMP")) %dopar%{
						cvRes <- rpartCVSingle(data, covars, k, cpTable, dropGrps, numCPLvls, minsplit, minbucket)
						return(list(cvRes))
					}
				}, finally = {
					parallel::stopCluster(cl) # Close the parallel connections
				}
		)
		errorRate <- lapply(cvRes, function(x)x[[1]])
		subTree <- lapply(cvRes, function(x)x[[2]])
	}else{
		errorRate <- vector("list", numCV)
		subTree <- vector("list", numCV)
		for(k in 1:numCV){
			cvRes <- rpartCVSingle(data, covars, k, cpTable, dropGrps, numCPLvls, minsplit, minbucket)
			errorRate[[k]] <- cvRes[[1]]
			subTree[[k]] <- cvRes[[2]]
		}
	}
	
	# Calculate the square root of the errors
	error <- sapply(errorRate, sqrt)
	
	# Calculate CI of MSE
	ciInfo <- matrix(NA, numCPLvls, 4)
	for(j in 1:numCPLvls){
		ciInfo[j, 1] <- mean(error[j,])
		ciInfo[j, 2:3] <- rpartCI(error[j,], 0.95)
		ciInfo[j, 4] <- sd(error[j,])/sqrt(ncol(error))
	}
	
	ciInfo <- cbind(ciInfo, rank(ciInfo[,1]))
	colnames(ciInfo) <- c("MSE", "Lower", "Upper", "SE", "Rank")
	
	# Add ci info back into cp table
	cpTable2 <- cbind(rpartRes$cptable, ciInfo)
	
	return(list(subTree=subTree, errorRate=do.call("cbind", errorRate), ciInfo=cpTable2))
}
