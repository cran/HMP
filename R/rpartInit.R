rpartInit <-
function(y, offset, parms, wt){
	hmp.pkg.env$EVAL_COUNT_RPART <- 1	# reset eval counts
	sfun <- function(yval, dev, wt, ylevel, digits ){
		paste(" mean=", round(mean(yval), 3), sep="")
	}
	environment(sfun) <- .GlobalEnv
	list(y=y, parms=NULL, numresp=1, numy=ncol(y), summary=sfun)
}
