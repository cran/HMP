plotRpartPerm <-
function(rawResults, rpartPermRes, numPerms){
	# Combine perms with real data
	allData <- rbind(rawResults, rpartPermRes)
	rownames(allData) <- 1:nrow(allData)
	
	par(mar=c(5, 4, 4, 5) + .1)
	
	# Make the inital plot
	plot(NULL, type="b", main="Number of Leaves vs Relative Error", lwd=2, 
			xlab="Number of Terminal Nodes", 
			ylab="Relative Error", 
			xlim=range(allData$Leafs, na.rm=TRUE, finite=TRUE),
			ylim=range(allData$RelErr, na.rm=TRUE, finite=TRUE)
	)
	# Add all permutation results
	for(i in 1:numPerms){
		tempData <- rpartPermRes[rpartPermRes$Tree == unique(rpartPermRes$Tree)[i],]
		lines(tempData$Leafs, tempData$RelErr, col="red", lty=2)
	}
	# Draw raw line
	lines(rawResults$Leafs, rawResults$RelErr, col="black", type="b", pch=16, lwd=3)
}
