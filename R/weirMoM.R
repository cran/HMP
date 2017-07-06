weirMoM <-
function(data, MoM, se=FALSE){
	numTaxa <- ncol(data)
	numSamp <- nrow(data)
	rowSumsData <- rowSums(data) + 0.000001
	colSumsData <- colSums(data)
	
	if(numSamp == 1)
		return(list(theta=0, se=0))
	
	MSP <- (numSamp-1)^(-1) * sum(rowSums((data/rowSumsData - matrix(rep(MoM, numSamp), numSamp, numTaxa, byrow=TRUE))^2) * rowSumsData)
	MSG <- (sum(colSumsData)-numSamp)^(-1) * sum(rowSums(data/rowSumsData * (1-data/rowSumsData)) * rowSumsData)
	nc <- 1/(numSamp-1) * (sum(rowSumsData)-sum(rowSumsData^2)/sum(rowSumsData))
	MoM.wh <- (MSP-MSG)/(MSP+(nc-1)*MSG)
	
	std.er <- NULL
	if(se)
		std.er <- sqrt(2 * (1-MoM.wh)^2/(numSamp-1) * ((1+(nc-1) * MoM.wh)/nc)^2)
	
	return(list(theta=MoM.wh, se=std.er))
}
