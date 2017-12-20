Plot.RM.Dotplot <-
function(group.data, groups, times, errorBars=TRUE, col=NULL, conf=.95, alpha=1){
	if(missing(group.data) || missing(groups) || missing(times))
		stop("group.data, groups and/or times are missing.")
	
	numSamps <- length(group.data)
	numGrps <- length(unique(groups))
	
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
		col <- rainbow(numGrps)
	### Add alpha to the colors
	col <- apply(sapply(col, grDevices::col2rgb)/255, 2, function(x){grDevices::rgb(x[1], x[2], x[3], alpha=alpha)})  
	
	if(errorBars){
		lattice::dotplot(params$Taxa ~ params$PI | paste("Time", params$Time), 
				pch=19, groups=params$Grp, col=col,
				ylab="Taxa", xlab="Fractional Abundance", 
				panel=lattice::panel.superpose, 
				panel.groups=function(x, y, subscripts, col, ...){
					lattice::panel.xyplot(x, y, ...)
					lattice::panel.segments(params$Lower[subscripts], y, params$Upper[subscripts], y, col=col)
				},
				key=list(
						text=list(as.character(unique(params$Grp))), 
						points=list(pch=19, col=col)
				)
		)
	}else{
		lattice::dotplot(params$Taxa ~ params$PI | paste("Time", params$Time), 
				pch=19, groups=params$Grp, col=col,
				ylab="Taxa", xlab="Fractional Abundance", 
				key=list(
						text=list(as.character(unique(params$Grp))), 
						points=list(pch=19, col=col)
				)
		)
	}
}
