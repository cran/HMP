DM.MoM <-
function(data){
	if(missing(data))
		stop("data missing.")
	
	pi.MoM <- colSums(data)/sum(data)
	theta.MoM <- weirMoM(data, pi.MoM)$theta
	gamma.MoM <- pi.MoM*((1-theta.MoM)/theta.MoM)
	
	# Set LL to Inf if we only have 1 sample
	if(nrow(data) == 1){
		loglikdm <- Inf
	}else{
		loglikdm <- loglikDM(data, gamma.MoM)
	}
	
	fit.MoM <- list(loglik=loglikdm, gamma=gamma.MoM, pi=pi.MoM, theta=theta.MoM)
	
	return(fit.MoM)
}
