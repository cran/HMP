Xmcupo.statistics.H <-
function(K, group.parameter){		
	n.groups <- length(group.parameter)
	index <- as.matrix(seq(1:n.groups))

	group.parameter.estimated <- list()
	for(x in index){# Dirichlet-Multinomial
		pi <- group.parameter[[x]][1:K]
		theta <- group.parameter[[x]][K+1]
		P <- length(group.parameter[[x]])
		nreads.data <- as.matrix(group.parameter[[x]][(K+2):P])
		data <- Dirichlet.multinomial(nreads.data,shape=pi*(1-theta)/theta)

		#MoM of theta and Unbiased estimator of pi
		pi.MoM <- apply(data,2,sum)/sum(apply(data,2,sum))
		theta.MoM <- weirMoM(data)
						
		group.parameter.estimated[[x]] <- c(pi.MoM,theta.MoM,t(nreads.data))			
	}
					
	#Xmcupo.statistics
	Xmcupo <- Xmcupo.statistics(group.parameter.estimated,K)								
}

