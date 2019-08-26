DM.Rpart.Base.Old <-
function(data, covars, plot=TRUE, minsplit=1, minbucket=1, cp=0){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	
	# Set the methods to use and call rpart
	methods <- list(init=rpartInit, eval=rpartEval, split=rpartSplitOld)
	rpartRes <- rpart::rpart(as.matrix(data) ~., data=covars, method=methods, minsplit=minsplit, minbucket=minbucket, cp=cp)
	
	cpInfo <- rpartRes$cptable
	size <- cpInfo[nrow(cpInfo), 2] + 1
	
	# Get split info from best tree
	splits <- NULL
	if(size > 1)
		splits <- rpartCS(rpartRes)
	
	# Plot the rpart results
	if(plot)
		suppressWarnings(rpart.plot::rpart.plot(rpartRes, type=2, extra=101, box.palette=NA, branch.lty=3, shadow.col="gray", nn=FALSE))
	
	return(list(cpTable=cpInfo, fullTree=rpartRes, bestTree=rpartRes, subTree=NULL, errorRate=NULL, size=size, splits=splits))
}
