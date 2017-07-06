Test.Paired <-
function(group.data, numPerms=1000, parallel=FALSE, cores=3){
	if(missing(group.data))
		stop("group.data is missing.")
	
	if(length(group.data) != 2)
		stop("group.data must have exactly 2 data sets.")
	
	if(numPerms <= 0)
		stop("The number of permutations must be an integer greater than 0.")
	
	# Make sure we have the same columns
	if(ncol(group.data[[1]]) != ncol(group.data[[2]])){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
	}
	
	# Check they have the same number of subjects
	numSub <- nrow(group.data[[1]])
	if(numSub != nrow(group.data[[2]]))
		stop("Groups must have the same number of subjects.")
	
	# Check row names match
	rNames1 <- rownames(group.data[[1]])
	rNames2 <- rownames(group.data[[2]])
	if(!all(rNames1 == rNames2)){ # check names in the same order
		if(all(rNames1 %in% rNames2)){ # check names match in wrong order
			group.data[[1]] <- group.data[[1]][order(rNames1),]
			group.data[[2]] <- group.data[[2]][order(rNames2),]
		}else{
			warning("Subject names do not match, assuming data is ordered correctly.")
		}
	}
	
	# Turn into abundances
	group.data[[1]] <- group.data[[1]]/rowSums(group.data[[1]])
	group.data[[2]] <- group.data[[2]]/rowSums(group.data[[2]])
	
	# Merge data1 and data2 together
	dataComb <- rbind(group.data[[1]], group.data[[2]])
	
	# Get the differences between the groups
	dataDiff <- group.data[[1]] - group.data[[2]]
	meanDiff <- apply(dataDiff, 2, mean)
	
	# Calculate the sum of squares
	obsDiff <- sum(meanDiff^2)
	
	# Permute the group membership
	if(parallel){
		cl <- parallel::makeCluster(cores) 
		doParallel::registerDoParallel(cl)
		
		tryCatch({ 
					permDiffs <- foreach::foreach(i=1:numPerms, .combine=c, .inorder=FALSE, .multicombine=TRUE) %dopar%{
						# Randomly swap group membership by reverseing difference sign
						swaps <- sample(c(1, -1), numSub, replace=TRUE)
						dataDiffTemp <- dataDiff * swaps
						meanDiffTemp <- apply(dataDiffTemp, 2, mean)
						
						# Calculate the sum of squares
						obsDiffTemp <- sum(meanDiffTemp^2)
						
						return(obsDiffTemp)
					}	
				}, finally = {
					parallel::stopCluster(cl) # Close the parallel connections
				}
		)
	}else{
		permDiffs <- rep(0, numPerms)
		for(i in 1:numPerms){ 	
			# Randomly swap group membership by reverseing difference sign
			swaps <- sample(c(1, -1), numSub, replace=TRUE)
			dataDiffTemp <- dataDiff * swaps
			meanDiffTemp <- apply(dataDiffTemp, 2, mean)
			
			# Calculate the sum of squares
			permDiffs[i] <- sum(meanDiffTemp^2)
		}
	}	
	
	# Calculate pvalue
	pval <- (sum(permDiffs >= obsDiff) + 1)/(numPerms + 1)
	
	return(pval)
}
