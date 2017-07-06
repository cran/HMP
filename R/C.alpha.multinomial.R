C.alpha.multinomial <-
function(data){
	if(missing(data))
		stop("data missing.")
	
	perNumReadsSubs <- rowSums(data)/sum(data)
	
	# Get T statistic
	Ts <- T.statistics(data)
	
	M.alpha <- diag(perNumReadsSubs)- as.matrix(perNumReadsSubs) %*% t(as.matrix(perNumReadsSubs))
	g <- sum(diag(M.alpha %*% M.alpha)) / sum(diag(M.alpha))
	df <- (ncol(data)-1)*((sum(diag(M.alpha)))^2) / (sum(diag(M.alpha %*% M.alpha)))
	
	# Get pvalue
	pval <- 1-pchisq(q=Ts/g, df=df, ncp=0, lower.tail=TRUE)
	
	GoF.test <- list("T statistics"=Ts, "p value"=pval)
	
	return(GoF.test)
}
