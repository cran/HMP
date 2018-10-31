rpartCI <-
function(vector, interval) {
	vec_sd <- sd(vector)
	numSamp <- length(vector)
	vec_mean <- mean(vector)
	
	# Error according to t distribution
	error <- qt((interval + 1)/2, df = numSamp - 1) * vec_sd/sqrt(numSamp)
	
	# Confidence interval as a vector
	res <- c("Lower"=vec_mean - error, "Upper"=vec_mean + error)
	return(res)
}
