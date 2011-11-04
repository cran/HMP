MC.Xoc.statistics <-
function(Nrs, MC, group.alphap, n.groups, type="hnull", siglev=0.05) {
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(group.alphap)){
		stop("group.alphap missing")
	}
	if(missing(n.groups)){
		stop("n.groups missing")
	}
	if(missing(MC)){
		stop("MC missing")
	}

	if(type=="ha") {
		taxaK=length(group.alphap[[1]])
	}else if(type=="hnull") {
		taxaK=length(group.alphap)
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}

	MCC=as.matrix(seq(1,1,length=MC))
	
	Xdct=t(apply(MCC,1,function(x) {Xoc.statistics.H(Nrs, group.alphap, n.groups, type)}))
	Xdc=t(Xdct)

	Xoc_pval=apply(Xdc, 2, function(t){q.alpha=qchisq(p=(1-siglev), 
		df=(n.groups-1)*taxaK, ncp=0, 
		lower.tail = TRUE); sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))})
}

