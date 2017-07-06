rpartEval <-
function(y, wt, parms){
	# Set a unique label
	label <- hmp.pkg.env$EVAL_COUNT_RPART
	hmp.pkg.env$EVAL_COUNT_RPART <- hmp.pkg.env$EVAL_COUNT_RPART + 1
	
	dev <- DM.MoM(y)$loglik * -1
	
	# Skip any infinite LL comparisons (makes lrt 0)
	if(dev == Inf || dev == -Inf)
		dev <- 0
	
	list(label=label, deviance=dev)
}
