MC.Xmc.statistics <-
function(Nrs, MC, pi0, group.pi, group.theta, type="hnull", siglev=0.05) {
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}
	if(missing(pi0) && type == "hnull"){
		stop("pi0 missing")
	}
	if(missing(group.theta)){
		stop("group.theta missing")
	}
	if(missing(group.pi) && type == "ha"){
		stop("group.pi missing")
	}

	MCC=as.matrix(seq(1,1,length=MC))
	Nrs=t(t(Nrs))
	n.groups=length(group.theta)
	index=as.matrix(seq(1:n.groups))
	K=length(pi0)
	
	group.parameter=list()
	
	if(type == "hnull"){
		for(i in index){
			group.parameter[[i]]=c(pi0,group.theta[i],Nrs)
		}
	}else if(type == "ha"){
		for(i in index){	
			group.parameter[[i]]=c(group.pi[i,],group.theta[i],Nrs)
		}
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}	
 
	Xmct=t(apply(MCC,1,function(x) {Xmc.statistics.Hnull.ha(K,pi0,group.parameter)}))
	Xmc=t(Xmct)
	
	dgf=length(group.theta)*(K-1)
	
	Xmc_pval=apply(Xmc, 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=dgf, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})
}

