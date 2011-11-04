Xoc.statistics.H <-
function(Nrs, group.alphap, n.groups, type){
	index=as.matrix(seq(1:n.groups))
	group.data.null<-list()
	for(x in index){ #Dirichlet-Multinomial
		if(type=="ha") {
			data=Dirichlet.multinomial(Nrs,shape=group.alphap[[x]]) 
		}else if(type=="hnull") {
			data=Dirichlet.multinomial(Nrs,shape=group.alphap)			
		}
		group.data.null[[x]] = data
	}
					
	#Xdc.statistics with MoM
	Xoc = Xoc.statistics(group.data.null, initscalar = 30, epsilon = 10^(-4))								
}

