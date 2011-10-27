Multinomial <-
function(Nrs, probs){
	if(missing(Nrs)){
		stop("Nrs missing")
	}
	if(missing(probs)){
		stop("probs missing")
	}

	Nrz=t(t(Nrs))
	Sample.counts=t(apply(Nrz,1,function(x){rmultinom(n=1,size=x,prob=probs)}))
	data=Sample.counts[, colSums(Sample.counts) != 0]
}

