MC.Xsc.statistics <-
function(Nrs, MC, fit, pi0=NULL, type="hnull", siglev=0.05) {
	if(missing(Nrs)){
		stop("Ns missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}
	if(missing(fit)){
		stop("fit missing")
	}

	MCC <- as.matrix(seq(1,1,length=MC))
	Nrs <- t(t(Nrs))

	Xsct <- t(apply(MCC,1,function(x) {Xsc.statistics.Hnull.Ha(Nrs, fit, type, pi0)}))
	Xsc <- t(Xsct)

	Xsc_pval <- apply(Xsc, 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=length(fit$pi)-1, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})
}

