DM.Rpart <-
function(data, covars, plot=TRUE, main="", minsplit=1, minbucket=1, cp=0){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	
	# Set the methods to use and call rpart
	methods <- list(init=rpartInit, eval=rpartEval, split=rpartSplit)
	res <- rpart::rpart(as.matrix(data) ~., data=covars, method=methods, minsplit=minsplit, minbucket=minbucket, cp=cp)
	
	# Plot the rpart results
	if(plot)
		suppressWarnings(rpart.plot::rpart.plot(res, main=main, extra=1))
	
	return(res)
}
