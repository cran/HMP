MC.Xmcupo.statistics <-
function(Nrs, MC, pi0, group.pi, group.theta, type="hnull", siglev=0.05) {
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}
	if(missing(group.theta)){
		stop("group.theta missing")
	}
	if(missing(pi0)){
		stop("pi0 missing")
	}
	if(missing(group.pi) && type == "ha"){
		stop("group.pi missing")
	}

	Nrs <- t(t(Nrs))
	MCC <- as.matrix(seq(1,1,length=MC))
	
	n.groups <- length(group.theta)
	index <- as.matrix(seq(1:n.groups))
	group.parameter <- list()
	K <- length(pi0)

	if(type == "hnull"){
		for(i in index){
			group.parameter[[i]] <- c(pi0,group.theta[i],Nrs)
		}
	}else if(type == "ha"){
		for(i in index){	
			group.parameter[[i]] <- c(group.pi[i,],group.theta[i],Nrs)
		}
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}
	
	Xmcupot <- t(apply(MCC,1,function(x) {Xmcupo.statistics.H(K,group.parameter)}))
	Xmcupo <- t(Xmcupot)

	dgf <- (length(group.theta)-1)*(K-1)

	Xmcupo_pval <- apply(Xmcupo, 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=dgf, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})	
}

