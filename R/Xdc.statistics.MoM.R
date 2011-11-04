Xdc.statistics.MoM <-
function(group.data){
	if(missing(group.data)){
		stop("group.data missing")
	}

	fit=lapply(group.data,DM.MoM)
	logliks=unlist(lapply(fit,function(x) x$loglik))
	groupData=NULL;n.groups=length(group.data)
	for(i in 1:n.groups){groupData=rbind(groupData,group.data[[i]])}
	fit.group=DM.MoM(groupData)	
	Xdc=-2*(fit.group$loglik- sum(logliks))
}

