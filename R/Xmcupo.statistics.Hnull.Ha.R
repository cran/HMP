Xmcupo.statistics.Hnull.Ha <-
function(groupParameter){		
	numGroups <- length(groupParameter)
	numTaxa <- length(groupParameter[[1]]$pi)
	
	genGroupParameter <- vector("list", numGroups)
	for(i in 1:numGroups){
		pi <- groupParameter[[i]]$pi
		theta <- groupParameter[[i]]$theta
		numReads <- groupParameter[[i]]$nrs
		
		# Generate a new set of data
		genData <- Dirichlet.multinomial(numReads, pi*(1-theta)/theta)
		genTotalReads <- sum(genData)
		genPi <- colSums(genData)/genTotalReads
		
		# Replace any 0 pi values with a small number
		# This will subtract that value from the other data so a total value of 1 is maintained
		if(any(genPi==0)){
			numZero <- sum(genPi==0)
			numNonZero <- numTaxa - numZero
			genPi[which(genPi!=0)] <- genPi[which(genPi!=0)] - numZero/(numNonZero*2*(genTotalReads+1))
			genPi[which(genPi==0)] <- 1/(2*(genTotalReads+1))
		}
		
		genTheta <- weirMoM(genData, genPi)$theta
		genGroupParameter[[i]] <- list(pi=genPi, theta=genTheta, nrs=numReads)			
	}
	
	# Get the Xmcupo stats for the generated data
	Xmcupo <- Xmcupo.statistics(genGroupParameter)		
	
	return(Xmcupo)
}
