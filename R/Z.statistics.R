Z.statistics <-
function(data){
	numTaxa <- ncol(data)
	numReadsTaxa <- colSums(data)
	numReadsSubs <- rowSums(data)
	totalReads <- sum(data)
	
	taxaSqSum <- sum(apply(data, 2, function(x){sum((x-1)*x)})/numReadsTaxa) 
	subSqSum <- sum(numReadsSubs*(numReadsSubs-1))
	
	denom <- sqrt(2*(numTaxa-1) * subSqSum)
	
	Zs <- (totalReads*taxaSqSum-subSqSum)/denom
	
	return(Zs)
}
