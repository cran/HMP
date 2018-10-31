Kullback.Leibler <-
function(group.data, plot=TRUE, main="Kullback Leibler Divergences", parallel=FALSE, cores=3){
	if(missing(group.data))
		stop("data missing.")
	
	# Check the number of groups
	numGrps <- length(group.data)
	if(numGrps < 2)
		stop("At least 2 data sets are required.")
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
	}
	
	# Make sure we have group names
	if(is.null(names(group.data))){
		grpNames <- paste("Data Set", 1:numGrps)
	}else{
		grpNames <- names(group.data)
	}
	
	# Add 1 so we don't ever get an all 0 comparison
	group.data <- lapply(group.data, function(x) x+1)  
	
	# Run dirmult on every group
	if(parallel){
		cl <- parallel::makeCluster(min(cores, numGrps)) 
		doParallel::registerDoParallel(cl)
		
		tryCatch({
					results <- foreach::foreach(i=1:numGrps, .combine=list, .multicombine=TRUE, .inorder=TRUE, .packages=c("dirmult")) %dopar%{
						param <- DM.MoM(group.data[[i]])
						return(param)
					}
				}, finally = {
					parallel::stopCluster(cl) # Close the parallel connections
				}
		)
	}else{
		results <- vector("list", numGrps)
		for(i in 1:numGrps)	
			results[[i]] <- DM.MoM(group.data[[i]])
	}
	
	# Get alpha for every group
	alpha <- lapply(results, function(x) x$gamma)
	names(alpha) <- grpNames
	
	# Get LL given alpha
	LL.vals <- sapply(results, function(x) x$loglik)
	
	# Get LL for every group using another alpha
	KLmat <- matrix(0, numGrps, numGrps)		
	for(i in 1:numGrps){
		for(j in i:numGrps){
			if(i == j)
				next
			KLval1 <- LL.vals[i] - loglikDM(group.data[[i]], alpha[[j]])
			KLval2 <- LL.vals[j] - loglikDM(group.data[[j]], alpha[[i]])
			KLmat[i, j] <- KLval1 + KLval2
			KLmat[j, i] <- KLval1 + KLval2
		}
	}
	colnames(KLmat) <- grpNames
	rownames(KLmat) <- grpNames
	
	if(plot){
		gplots::heatmap.2(as.matrix(KLmat), dendrogram="both", Rowv=TRUE, Colv=TRUE, 
				trace="none", symm=TRUE, margins=c(12, 9), density.info="none",
				main=main
		)
	}
	
	return(KLmat)
}
