Xsc.statistics.Hnull.Ha <-
function(Nrs, fit, type, pi0){
	# Dirichlet-Multinomial
	data=Dirichlet.multinomial(Nrs,shape=fit$gamma)
					
	#MoM of theta and Unbiased estimator of pi
	fit.null<-DM.MoM(data)					
		
	#Xsc.statistics
	if(type == "hnull"){
		Xsc=Xsc.statistics(pi1=fit.null$pi,theta=fit.null$theta,Nrs,pi0=fit$pi)
	}else if(type == "ha"){
		Xsc=Xsc.statistics(pi1=fit.null$pi,theta=fit.null$theta,Nrs,pi0=pi0)
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}

}

