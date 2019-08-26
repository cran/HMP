rpartSplitOld <-
function(y, wt, x, parms, continuous){
	# Get initial LL
	LL <- DM.MoM(y)$loglik
	
	# Determine what we are comparing
	if(continuous){
		numTests <- length(x) - 1
		dir <- rep(-1, numTests)
	}else{
		uniqX <- sort(unique(x))
		numTests <- length(uniqX) - 1
		dir <- uniqX
	}
	
	# Run through every comparison
	LRT <- rep(0, numTests)
	for(i in 1:numTests){
		if(continuous){
			grp1 <- y[1:i,, drop=FALSE]
			grp2 <- y[-c(1:i),, drop=FALSE]
		}else{
			grp1 <- y[x == uniqX[i],, drop=FALSE]
			grp2 <- y[x != uniqX[i],, drop=FALSE]
		}
		# Skip any 1 subject groups
		if(nrow(grp1) == 1 || nrow(grp2) == 1)
			next
		
		LLgrp1 <- DM.MoM(grp1)$loglik
		LLgrp2 <- DM.MoM(grp2)$loglik
		
		# Skip any infinite LL comparisons (makes lrt 0)
		if(LLgrp1 == Inf || LLgrp2 == Inf)
			next
		
		LRT[i] <- -2*(LL-LLgrp1-LLgrp2)
	}
	ret <- list(goodness=LRT, direction=dir)
	
	return(ret)
}
