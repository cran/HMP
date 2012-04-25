Dirichlet.multinomial <-
function(Nrs, shape){
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(shape)){
		stop("shape missing")
	}
	if(Nrs <= 0 || shape <=0){
		stop("Nrs and shape must be positive.")
	}

	Nrz <- t(t(Nrs))
	Sample.counts <- t(apply(Nrz,1,function(x){rmultinom(n=1,size=x,prob=rdirichlet(1,shape))}))
	data <- Sample.counts
}

