MC.Xsc.statistics <-
function(Nrs, numMC=10, fit, pi0=NULL, type="ha", siglev=0.05, MC=NULL) {
	if(missing(Nrs) || missing(fit))
		stop("Nrs and/or fit missing.")
	if(is.null(pi0) && tolower(type) == "ha")
		stop("pi0 cannot be null with type 'ha'.")
	if(tolower(type) != "ha" && tolower(type) != "hnull")
		stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
	
	# Check if someone is still using MC
	if(!is.null(MC)){
		warning("'MC' is deprecated. It has been replaced with numMC. View the help files for details.")
		numMC <- MC
	}
	
	# Get all the XSC values
	XscStatVector <- rep(0, numMC)
	for(i in 1:numMC)
		XscStatVector[i] <- Xsc.statistics.Hnull.Ha(Nrs, fit, type, pi0)
	
	# Remove NAs
	XscStatVector <- XscStatVector[!is.na(XscStatVector)]
	
	# Get a reference value from the real data
	qAlpha <- qchisq(p=(1-siglev), df=length(fit$pi)-1, ncp=0, lower.tail=TRUE)
	
	# Calculate pvalues
	pval <- (sum(XscStatVector > qAlpha) + 1)/(length(XscStatVector) + 1)
	
	return(pval)
}
