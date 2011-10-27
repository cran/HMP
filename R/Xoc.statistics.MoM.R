Xoc.statistics.MoM <-
function(group.data){
	if(missing(group.data)){
		stop("group.data missing")
	}

	fit=lapply(group.data,DM.MoM)
	logliks=unlist(lapply(fit,function(x) x$loglik))
	pigroups=(lapply(fit,function(x) x$pi))
	
	groupData=NULL;n.groups=length(group.data)
	for(i in 1:n.groups){
		groupData=rbind(groupData,group.data[[i]])
	}
	
	fit.group=DM.MoM(groupData)
	thetagroup=fit.group$theta
	indexp=as.matrix(1:n.groups)
	equal.theta.loglik=sum(apply(indexp,1,function(x){loglikDM(group.data[[x]],pigroups[[x]]*(1-thetagroup)/thetagroup)}))
	Xdc=-2*(equal.theta.loglik- sum(logliks))	
}

