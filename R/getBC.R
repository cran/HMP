getBC <-
function(data){
	dataPer <- data/rowSums(data)
	bcDist <- vegan::vegdist(dataPer, method="bray")
	nonMetricMDS <- MASS::isoMDS(bcDist, trace=FALSE)
	mdsPoints <- vegan::postMDS(nonMetricMDS$points, bcDist)
	mds <- vegan::scores(mdsPoints)
	
	return(mds[, 1:2])
}
