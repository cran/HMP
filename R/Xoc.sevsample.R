Xoc.sevsample <-
function(group.data,initscalar=30,epsilon=10^(-4)){
	if(missing(group.data)){
		stop("group.data missing")
	}

	Xoc <- Xoc.statistics(group.data,initscalar,epsilon)
	groups <- length(group.data)
	taxaK <- dim(group.data[[1]])[2]
	p.value <- 1-pchisq(q=Xoc, df=(groups-1)*taxaK, ncp=0, lower.tail = TRUE)	
	sev.overd.test <- list(Xoc[[1]],p.value)
	names(sev.overd.test) <- c('Xoc statistics','p value')
	return(sev.overd.test)			
}

