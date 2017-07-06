T.statistics <-
function(data){	
	numReadsTaxa <- colSums(data)
	numReadsSubs <- rowSums(data)
	totalReads <- sum(data)
	
	Ts <- sum(colSums((data - (numReadsSubs%*%t(numReadsTaxa))/totalReads)^2) / numReadsTaxa)
	
	return(Ts)
}
