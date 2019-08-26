Data.filter <-
function(data, order.type="data", minReads=0, numTaxa=NULL, perTaxa=NULL){
	if(missing(data))
		stop("data is missing.")
	if(tolower(order.type) != "data" && tolower(order.type) != "sample")
		stop(sprintf("'%s' not recognized, order.type must be 'data' or 'sample'", as.character(order.type)))
	
	# Check if numTaxa or perTaxa is being used
	if(!is.null(numTaxa) && !is.null(perTaxa))
		stop("numTaxa and perTaxa cannot be used at the same time")
	if(!is.null(numTaxa)){
		if(numTaxa > ncol(data) || numTaxa <= 0)
			stop(sprintf("numTaxa must be between 0 and %i.", ncol(data)))
	}
	if(!is.null(perTaxa)){
		if(perTaxa >= 1 || perTaxa <= 0)
			stop("perTaxa must be between 0 and 1.")
	}
	if(is.null(numTaxa) && is.null(perTaxa))
		numTaxa <- ncol(data)
	
	taxaNames <- colnames(data)
	
	# Drop all subjects that don't have enough reads
	data <- data[rowSums(data)>minReads,, drop=FALSE]
	if(nrow(data) < 2)
		stop("minReads is too large and is excluding too many samples.  Please try lowering its value.")
	
	# Drop all 0 taxa
	data <- data[,colSums(data)>0, drop=FALSE]
	
	# Order the data based on order.type
	if(tolower(order.type) == "sample"){
		data <- t(apply(data, 1, function(x){x[order(x, decreasing=TRUE)]}))
	}else{
		data <- data[,order(colSums(data), decreasing=TRUE)]
	}
	
	# Use a percentage based approach to find the number of taxa to collapse
	if(!is.null(perTaxa)){
		perNumReadsTaxa <- colSums(data)/sum(data)
		cumSumReads <- cumsum(perNumReadsTaxa)
		taxaAboveThrs <- which(cumSumReads > perTaxa)
		if(length(taxaAboveThrs) == 0){
			numTaxa <- 1
		}else{
			numTaxa <- min(taxaAboveThrs)
		}
	}
	
	if(numTaxa >= ncol(data)){
		retData <- data
	}else{
		# Pull out the taxa we want to collapse
		otherData <- data[,-c(1:numTaxa), drop=FALSE]
		
		# Put the data back together and relabel
		retData <- cbind(data[,1:numTaxa], Other=rowSums(otherData))
	}
	
	return(retData)
}
