Xoc.statistics <-
function(group.data,initscalar=30,epsilon=10^(-4)){
	if(missing(group.data)){
		stop("group.data missing")
	}

	if(is.numeric(initscalar)){
		fit <- lapply(group.data,dirmult,epsilon=epsilon,initscalar=initscalar,trace=FALSE)
	}else{
		fit <- lapply(group.data,function(x,epsilon){dirmult(x,init=DM.MoM(x)$gamma,epsilon,trace=FALSE)},epsilon=epsilon)
	}

	thetas <- unlist(lapply(fit,function(x) x$theta))
	inipi <- lapply(fit,function(x) x$pi)
	logliks <- unlist(lapply(fit,function(x) x$loglik))

	fit1 <- equalTheta(group.data,theta=mean(thetas),epsilon=epsilon,trace=FALSE,initPi=inipi)
	Xoc <- -2*(fit1$loglik-sum(logliks))
}

