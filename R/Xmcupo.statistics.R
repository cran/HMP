Xmcupo.statistics <-
function(group.parameter, K){
	if(missing(group.parameter)){
		stop("group.parameter missing")
	}else if(missing(K)){
		stop("K missing")
	}

	n.groups=length(group.parameter)
	index=as.matrix(seq(1:n.groups))

	Xscg=apply(index,1,function(x){
		pi=group.parameter[[x]][1:K];
		theta=group.parameter[[x]][K+1];
		P=length(group.parameter[[x]])
		nreads.data=group.parameter[[x]][(K+2):P];
		N_1Cj=((theta*(sum(nreads.data^2)-sum(nreads.data))+sum(nreads.data))/(sum(nreads.data))^2)
		Out1=c(pi,N_1Cj)
		Out1
	})
	
	pi0=(Xscg[1:K,]%*%as.matrix(1/Xscg[K+1,]))/sum(1/Xscg[K+1,])
	Xmcupo=sum((1/Xscg[K+1,])*apply((Xscg[1:K,]-pi0%*%matrix(1,1,n.groups))^2,2,function(x) sum(x/pi0)))		
}

