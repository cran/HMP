Dirichlet.multinomial <-
function(Nrs, shape){
	if(missing(Nrs) || missing(shape))
		stop("Nrs and/or shape missing.")
	
	# Create the data from the rmultinom
	dmData <- matrix(0, length(Nrs), length(shape))
	for(i in 1:length(Nrs))	
		dmData[i,] <- stats::rmultinom(1, Nrs[i], dirmult::rdirichlet(1, shape))
	
	# Label the created data
	colnames(dmData) <- paste("Taxa", 1:ncol(dmData))
	rownames(dmData) <- paste("Sample", 1:nrow(dmData))
	
	return(dmData)
}
