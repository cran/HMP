ZT.statistics <-
function(Nrs, fit, type){
	if(type == "hnull"){
		data=Multinomial(Nrs,probs=fit$pi)
	}else if(type == "ha"){
		data=Dirichlet.multinomial(Nrs,shape=fit$gamma)
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}

	#C(alpha) test-statistics
	Z=Z.statistics(data)

	#C(alpha) T test-statistics
	T=T.statistics(data)
	ZT=c(Z,T)
}

