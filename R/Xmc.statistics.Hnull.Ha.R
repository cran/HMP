Xmc.statistics.Hnull.Ha <-
function(groupParameter, pi0){	
	numGroups <- length(groupParameter)	
	numTaxa <- length(pi0)
	
	genGroupParameter <- vector("list", numGroups)
	for(i in 1:numGroups){
		pi <- groupParameter[[i]]$pi
		theta <- groupParameter[[i]]$theta
		numReads <- groupParameter[[i]]$nrs
		
		# Generate a new set of data
		genData <- Dirichlet.multinomial(numReads, pi*(1-theta)/theta)				
		genPi <- colSums(genData)/sum(genData)
		genTheta <- weirMoM(genData, genPi)$theta
		
		genGroupParameter[[i]] <- list(pi=genPi, theta=genTheta, nrs=numReads)		
	}
	
	# Get the Xmc stats for the generated data
	Xmc <- Xmc.statistics(genGroupParameter, pi0)	
	
	return(Xmc)
}
