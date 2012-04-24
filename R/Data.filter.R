Data.filter <-
function(data, order.type, reads.crit, K){
	if(missing(data)){
		stop("data missing")
	}else if(missing(order.type)){
		stop("order.type missing")
	}else if(missing(reads.crit)){
		stop("reads.crit missing")
	}else if(missing(K)){
		stop("K missing")
	}

	if((K + 1) > ncol(data)){
		stop(paste("K is too large.  It must be smaller than ", ncol(data)-1, ".", sep=""))
	}
	if(reads.crit >= max(rowSums(data))){
		stop(paste("reads.crit is too large.  It must be smaller than ", max(rowSums(data)), ".", sep=""))
	}

	#Order data: if order.type == sample, then every sample is ordered based on its frequency
	#otherwise samples are ordered based on taxa total counts across samples
	ifelse(order.type=="sample", data<-t(apply(data,1,function(x){x[order(x,decreasing=TRUE)]})), data<-data[,order(apply(data,2,sum),decreasing=TRUE)])

	#Selecting samples with more than reads.crit reads
	data <- data[apply(data,1,sum)>reads.crit,]

	#Removing empty ordered taxa
	data <- data[,apply(data,2,sum)>0]

	#Selecting the K most taxa
	P <- dim(data)[1];Taxa=dim(data)[2]
	dataK <- cbind(data[,1:K], as.matrix(apply(as.matrix(data[,(K+1):Taxa]),1,sum)))
}

