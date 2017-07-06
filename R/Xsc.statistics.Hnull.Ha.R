Xsc.statistics.Hnull.Ha <-
function(Nrs, fit, type, pi0){
	# Generate a new set of data
	genData <- Dirichlet.multinomial(Nrs, fit$gamma)
	fit.gen <- DM.MoM(genData)			
	
	tempPi <- fit$pi
	if(tolower(type) == "ha")
		tempPi <- pi0
	
	# Calculate Xsc stat
	xsc <- Xsc.statistics(fit.gen$pi, fit.gen$theta, Nrs, tempPi)
	
	return(xsc)
}
