ZT.statistics.Hnull.Ha <-
function(Nrs, fit, type){
	if(tolower(type) == "hnull"){
		genData <- Multinomial(Nrs, fit$pi)
	}else{
		genData <- Dirichlet.multinomial(Nrs, fit$gamma)
	}
	
	ZT <- c(Z.statistics(genData), T.statistics(genData))
	
	return(ZT)
}
