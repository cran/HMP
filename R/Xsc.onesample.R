Xsc.onesample <-
function(data, pi0){
	if(missing(data)){
		stop("data missing")
	}else if(missing(pi0)){
		stop("pi0 missing")
	}
	
	nreads=(apply(data,1,sum))
	fit.MoM=DM.MoM(data)
	MoM.theta=fit.MoM$theta
	MoM.pi=fit.MoM$pi
	
	rank.Bj= sum(pi0>0)-1 #number of non-zero elements of pi0 minus 1 is equal to rank(Bj)
	Xsc=Xsc.statistics(pi1=MoM.pi,theta=MoM.theta,nreads.data=nreads,pi0)
	
	p.value=1-pchisq(q=Xsc, df=rank.Bj, ncp=0, lower.tail = TRUE)

	RAD.mean.test=list(Xsc,p.value);names(RAD.mean.test)<-c('Xsc statistics','p value')
	return(RAD.mean.test)			
}

