DM.Rpart.Perm <-
function(data, covars, plot=TRUE, numPerms=1000, parallel=FALSE, cores=3, minsplit=1, minbucket=1, cp=0){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	
	if(numPerms <= 0)
		stop("The number of permutations must be an integer greater than 0.")
	
	numSub <- nrow(data) 
	
	# Run and prune an rpart tree with raw data
	res <- DM.Rpart(data, covars, FALSE, "", minsplit, minbucket, cp)
	rawResults <- pruneRpart(res, data, "Raw")
	
	### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Run permutations
	if(parallel){
		cl <- parallel::makeCluster(min(cores, numPerms)) 
		doParallel::registerDoParallel(cl)
		tryCatch({ 
					rpartPermRes <- foreach::foreach(i=1:numPerms, .combine=rbind, .multicombine=TRUE, .inorder=FALSE, .packages=c("rpart", "HMP")) %dopar%{
						# Run and prune an rpart tree with permuted data                    
						permData <- data[sample(numSub, numSub),, drop=FALSE]
						resPerm <- DM.Rpart(permData, covars, FALSE, "", minsplit, minbucket, cp)
						tempResults <- pruneRpart(resPerm, permData, i)
						
						return(tempResults)
					}
				}, finally = {
					parallel::stopCluster(cl) # Close the parallel connections
				}
		)
	}else{
		rpartPermRes <- matrix(0, 0, 4)
		for(i in 1:numPerms){
			# Run and prune an rpart tree with permuted data
			permData <- data[sample(numSub, numSub),, drop=FALSE]
			resPerm <- DM.Rpart(permData, covars, FALSE, "", minsplit, minbucket, cp)
			tempResults <- pruneRpart(resPerm, permData, i)
			
			rpartPermRes <- rbind(rpartPermRes, tempResults)
		}
	}
	
	# Plot the permutations vs the real data
	if(plot)
		plotRpartPerm(rawResults, rpartPermRes, numPerms)
	
	# Calculate pvalues
	pvals <- calcRpartPval(rawResults, rpartPermRes, numPerms)
	
	ret <- list(rawTree=res, rawPrune=rawResults, permPrune=rpartPermRes, pvals=pvals)
	return(ret)
}
