DM.MoM <-
function(data){
	if(missing(data)){
		stop("data missing")
	}

	pi.MoM <- apply(data,2,sum)/sum(apply(data,2,sum))
	theta.MoM <- weirMoM(data)
	gamma.MoM <- pi.MoM*((1-theta.MoM)/theta.MoM)
	loglikdm <- loglikDM(data,gamma.MoM)
	fit.MoM <- list(loglikdm,gamma.MoM,pi.MoM,theta.MoM)
	names(fit.MoM) <- c('loglik','gamma','pi','theta')
	return(fit.MoM)
}

