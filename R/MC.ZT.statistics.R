MC.ZT.statistics <-
function(Nrs, MC, fit, type="hnull", siglev=0.05) {
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}
	if(missing(fit)){
		stop("fit missing")
	}

	MCC <- as.matrix(seq(1,1,length=MC))
	Nrs <- t(t(Nrs))
	ZTt <- t(apply(MCC,1,function(x) {ZT.statistics(Nrs, fit, type)}))
	
	tpval <- apply(as.matrix(ZTt[,2]), 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=length(fit$pi)-1, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})

	zpval <- apply(as.matrix(ZTt[,1]), 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=length(fit$pi)-1, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})

	return(cbind(zpval, tpval))
}

