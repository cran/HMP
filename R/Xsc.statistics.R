Xsc.statistics <-
function(pi1, theta, nreads.data ,pi0){
	if(missing(pi1)){
		stop("pi1 missing")
	}else if(missing(theta)){
		stop("theta missing")
	}else if(missing(nreads.data)){
		stop("nreads.data missing")
	}else if(missing(pi0)){
		stop("pi0 missing")
	}

	Bj <- ((theta*(sum(nreads.data^2)-sum(nreads.data))+sum(nreads.data))/(sum(nreads.data))^2)*(diag(as.vector(pi0))-pi0%*%t(pi0))
	Xsc <- t(pi1-pi0)%*%ginv(Bj,tol = sqrt(.Machine$double.eps))%*%(pi1-pi0)		
}

