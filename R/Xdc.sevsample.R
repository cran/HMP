Xdc.sevsample <-
function(group.data,initscalar=30, epsilon=10^(-4)){
	if(missing(group.data)){
		stop("group.data missing")
	}

	Xdc <- Xdc.statistics(group.data,initscalar,epsilon)
	groups <- length(group.data)
	taxaK <- dim(group.data[[1]])[2]
	p.value <- 1-pchisq(q=Xdc, df=(groups-1)*taxaK, ncp=0, lower.tail = TRUE)				
	xdc.sevsamp.test <- list(Xdc[[1]],p.value);names(xdc.sevsamp.test)<-c('Xdc statistics','p value')
			return(xdc.sevsamp.test)	
}

