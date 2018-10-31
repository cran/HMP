DM.Rpart <-
function(data, covars, plot=TRUE, minsplit=1, minbucket=1, cp=0, numCV=10, numCon=100, parallel=FALSE, cores=3){
	if(missing(data) || missing(covars))
		stop("data and/or covars are missing.")
	
	if(numCV < 2){
		ret <- DM.Rpart.Base(data, covars, plot, minsplit, minbucket, cp)
	}else if(numCon < 2){
		ret <- DM.Rpart.CV(data, covars, plot, minsplit, minbucket, cp, numCV)
	}else{
		ret <- DM.Rpart.CV.Consensus(data, covars, plot,  minsplit, minbucket, cp, numCV, numCon, parallel, cores)
	}
	
	return(ret)
}
