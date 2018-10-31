DM.Rpart.CV <-
function(data, covars, plot=TRUE, minsplit=1, minbucket=1, cp=0, numCV=10){
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
	
	cvRes <- rpartCV(data, covars, rpartRes, minsplit, minbucket, numCV)
	
	# Calculate the best tree
	ciInfo <- cvRes$ciInfo
	bestTreeLoc <- which(ciInfo[,4] == min(ciInfo[,4]))
	bestTreeLoc <- bestTreeLoc[length(bestTreeLoc)]
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
