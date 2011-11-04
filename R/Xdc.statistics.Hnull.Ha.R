Xdc.statistics.Hnull.Ha <-
function(alphap, Nrs, n.groups, type, Est){				
	group.data.null<-list()
	index=as.matrix(seq(1:n.groups))	
	
	if(type == "hnull"){	
		for(x in index){	# Dirichlet-Multinomial
			data=Dirichlet.multinomial(Nrs,shape=alphap) 
			group.data.null[[x]]=data
		}
	}else if(type == "ha"){
		for(x in index){
			data=Dirichlet.multinomial(Nrs[x,],shape=alphap[x,])
			group.data.null[[x]]=data
		} 					
	}else{
		stop(paste("Can't find type ", type, sep=""))
	}
					
	#Xdc.statistics with MoM
	if(Est == "MLE"){
		Xdc <- Xdc.statistics(group.data.null)
	}
	else if(Est == "MoM"){
		Xdc <- Xdc.statistics.MoM(group.data.null)
	}
	else{
		stop(paste("Can't find est ", Est, sep=""))
	}
}

