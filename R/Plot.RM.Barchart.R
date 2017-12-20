Plot.RM.Barchart <-
function(group.data, groups, times, plotByGrp=TRUE, col=NULL, conf=.95){
	if(missing(group.data) || missing(groups) || missing(times))
		stop("group.data, groups and/or times are missing.")
	
	numSamps <- length(group.data)
	
	### Get the pi params
	myEst <- Est.PI(group.data, conf)
	params <- myEst$MLE$params
	
	### Add the group and time information to the params
	myGroups <- NULL
	myTimes <- NULL
	for(i in 1:numSamps){
		myGroups <- c(myGroups, rep(groups[i], ncol(group.data[[1]])))
		myTimes <- c(myTimes, rep(times[i], ncol(group.data[[1]])))
	}
	params$Grp <- as.character(myGroups)
	params$Time <- as.character(myTimes)
	
	if(is.null(col))
		col <- rainbow(length(unique(params$Taxa)))
	
	if(plotByGrp){
		lattice::barchart(params$PI ~ params$Time | paste("Group", params$Grp), 
				ylab="Fractional Abundance", xlab="Time", 
				stack=TRUE, groups=params$Taxa, col=col,
				key=list(
						text=list(levels(params$Taxa)), 
						points=list(pch=19, col=col),
						columns=5
				)
		)
	}else{
		lattice::barchart(params$PI ~ params$Grp | paste("Time", params$Time),
				ylab="Fractional Abundance", xlab="Time", 
				stack=TRUE, groups=params$Taxa, col=col,
				key=list(
						text=list(levels(params$Taxa)), 
						points=list(pch=19, col=col),
						columns=5
				)
		)
	}
}
