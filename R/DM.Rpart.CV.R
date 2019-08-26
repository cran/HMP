DM.Rpart.CV <-
function(data, covars, plot=TRUE, minsplit=1, minbucket=1, cp=0, numCV=10, parallel=FALSE, cores=3, use1SE=FALSE, lowerSE=TRUE){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	if(numCV < 2)
		stop("numCV must be at least 2.")
	
	# Run initial Rpart
	rpartBase <- DM.Rpart.Base(data, covars, FALSE, minsplit, minbucket, cp)
	rpartRes <- rpartBase$fullTree
	
	# Check for a valid starting tree
	if(nrow(rpartRes$cptable) == 1){
		warning("No splits in the data.")
		return(rpartBase)
	}
	
	cvRes <- rpartCV(data, covars, rpartRes, minsplit, minbucket, numCV, parallel, cores)
	
	# Calculate the best tree
	ciInfo <- as.data.frame(cvRes$ciInfo)
	
	# Find the tree with the lowest MSE
	minMSE <- min(ciInfo$MSE)
	lowestMSELoc <- which(ciInfo$MSE == minMSE)[1]
	
	if(use1SE){	
		# Find which trees are within 1 SE of the lowest mse tree
		cutoffU <- ciInfo$MSE[lowestMSELoc] + ciInfo$SE[lowestMSELoc]
		cutoffL <- ciInfo$MSE[lowestMSELoc] - ciInfo$SE[lowestMSELoc]
		ciInfo$within1SE <- ifelse(ciInfo$MSE <= cutoffU & ciInfo$MSE >= cutoffL, 1, 0)   
		
		# Find the smallest/biggest tree within 1 SE
		within <- which(ciInfo$within1SE == 1)
		if(lowerSE){
			bestTreeLoc <- min(within)
		}else{
			bestTreeLoc <- max(within)
		}
	}else{
		bestTreeLoc <- lowestMSELoc
	}
	
	# Pull out the best tree
	size <- ciInfo[bestTreeLoc, 2] + 1
	best <- rpart::prune(rpartRes, cp=ciInfo[bestTreeLoc, 1])
	
	# Get split info from best tree
	splits <- NULL
	if(size > 1)
		splits <- rpartCS(best)
	
	if(plot)
		suppressWarnings(rpart.plot::rpart.plot(best, type=2, extra=101, box.palette=NA, branch.lty=3, shadow.col="gray", nn=FALSE))
	
	return(list(cpTable=ciInfo, fullTree=rpartRes, bestTree=best, subTree=cvRes$subTree, errorRate=cvRes$errorRate, size=size, splits=splits))
}
