Multinomial <-
function(Nrs, probs){
	if(missing(Nrs) || missing(probs))
		stop("Nrs and/or probs missing.")
	
	# Create the data from the rmultinom
	mData <- matrix(0, length(Nrs), length(probs))
	for(i in 1:length(Nrs))	
		mData[i,] <- stats::rmultinom(1, Nrs[i], probs)
	
	# Label the created data
	colnames(mData) <- paste("Taxa", 1:ncol(mData))
	rownames(mData) <- paste("Sample", 1:nrow(mData))
	
	return(mData)
}
