MC.Xdc.statistics <-
function(Nrs, MC, alphap, n.groups, type="hnull", siglev=0.05, Est="MLE") {
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(alphap)){
		stop("alphap missing")
	}
	if(missing(n.groups)){
		stop("n.groups missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}
		
	Nrs=t(t(Nrs))
	MCC=as.matrix(seq(1,1,length=MC))
		
	Xdct=t(apply(MCC,1,function(x) {Xdc.statistics.Hnull.Ha(alphap,Nrs,n.groups,type, Est)}))
	Xdc=t(Xdct)

	if(type=="hnull"){
		K <- length(alphap)
	}else if(type=="ha"){
		K <- length(alphap[[1]])
	}

 	dgf=(n.groups-1)*K
	Xdc_pval=apply(Xdc, 2, function(t){q.alpha=qchisq(p=(1-siglev), df=dgf, 
		ncp=0, lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})
}

