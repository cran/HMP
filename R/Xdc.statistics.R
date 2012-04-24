Xdc.statistics <-
function(group.data,initscalar=30,epsilon=10^(-4)){
	if(missing(group.data)){
		stop("group.data missing")
	}

	if(is.numeric(initscalar)){
		fit <- lapply(group.data,dirmult,epsilon=epsilon,initscalar=initscalar,trace=FALSE)
	}else{
		fit <- lapply(group.data,function(x,epsilon){dirmult(x,init=DM.MoM(x)$gamma,epsilon,trace=FALSE)},epsilon=epsilon)
	}

	logliks <- unlist(lapply(fit,function(x) x$loglik))
	
	groupData <- NULL
	n.groups <- length(group.data)
	for(i in 1:n.groups){
		groupData <- rbind(groupData,group.data[[i]])
	}
	
	fit.group <- dirmult(groupData,init=DM.MoM(groupData)$gamma, epsilon=epsilon, trace=FALSE)	
	Xdc <- -2*(fit.group$loglik- sum(logliks))
}

