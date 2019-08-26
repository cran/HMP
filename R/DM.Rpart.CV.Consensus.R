DM.Rpart.CV.Consensus <-
function(data, covars, plot=TRUE, minsplit=1, minbucket=1, cp=0, numCV=10, numCon=100, parallel=FALSE, cores=3, use1SE=FALSE, lowerSE=TRUE){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	if(numCV < 2)
		stop("numCV must be at least 2.")
	if(numCon < 2)
		stop("numCon must be at least 2.")
	
	if(parallel){
		cl <- parallel::makeCluster(min(cores, numCon)) 
		doParallel::registerDoParallel(cl)
		
		tryCatch({
					results <- foreach::foreach(i=1:numCon, .combine=append, .multicombine=FALSE, .inorder=FALSE, .errorhandling="pass", .packages=c("rpart", "HMP")) %dopar%{
						cvList <- DM.Rpart.CV(data, covars, FALSE, minsplit, minbucket, cp, numCV, FALSE, 1, use1SE, lowerSE)
						return(list(cvList))
					}
				}, finally = {
					parallel::stopCluster(cl) # Close the parallel connections
				}
		)
	}else{
		results <- vector("list", numCon)
		for(i in 1:numCon)	
			results[[i]] <- DM.Rpart.CV(data, covars, FALSE, minsplit, minbucket, cp, numCV, FALSE, 1, use1SE, lowerSE)
	}
	
	# Combine cv results
	MSETab <- do.call("cbind", lapply(results, function(x){x$cpTable[,4]}))
	rankTab <- do.call("cbind", lapply(results, function(x){x$cpTable[,8]}))
	ciInfo <- cbind(
			results[[1]]$cpTable[,1:3],
			"MeanMSE"=rowMeans(MSETab), 
			"sdMSE"=apply(MSETab, 1, sd), 
			"MeanRank"=rowMeans(rankTab), 
			"sdRank"=apply(rankTab, 1, sd)
	)
	
	# Find the tree with the lowest MSE
	minMSE <- min(ciInfo$MeanMSE)
	bestTreeLoc <- which(ciInfo$MeanMSE == minMSE)[1]
	
	# Pull out the best tree
	size <- ciInfo[bestTreeLoc, 2] + 1
	best <- rpart::prune(results[[1]]$fullTree, cp=ciInfo[bestTreeLoc, 1])
	
	# Get split info from best tree
	splits <- NULL
	if(size > 1)
		splits <- rpartCS(best)
	
	if(plot)
		suppressWarnings(rpart.plot::rpart.plot(best, type=2, extra=101, box.palette=NA, branch.lty=3, shadow.col="gray", nn=FALSE))
	
	return(list(cpTable=ciInfo, fullTree=results[[1]]$fullTree, bestTree=best, subTree=NULL, errorRate=NULL, size=size, splits=splits))
}
