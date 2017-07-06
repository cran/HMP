Xdc.sevsample <-
function(group.data, epsilon=10^(-4), est="mom"){
	if(missing(group.data))
		stop("group.data missing.")
	if(tolower(est) != "mle" && tolower(est) != "mom")
		stop(sprintf("Est '%s' not found. Est must be 'mle' or 'mom'.", as.character(est)))
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
		numTaxa <- ncol(group.data[[1]])
	}
	
	numGroups <- length(group.data)
	
	# Get Xdc and calculate pvalue
	if(tolower(est) == "mle"){
		Xdc <- Xdc.statistics(group.data, epsilon)
	}else{
		Xdc <- Xdc.statistics.MoM(group.data)
	}
	pval <- 1-pchisq(q=Xdc, df=(numGroups-1)*numTaxa, ncp=0, lower.tail=TRUE)		
	
	xdc.sevsamp.test <- list("Xdc statistics"=Xdc, "p value"=pval)
	
	return(xdc.sevsamp.test)	
}
