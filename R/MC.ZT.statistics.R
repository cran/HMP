MC.ZT.statistics <-
function(Nrs, numMC=10, fit, type="ha", siglev=0.05) {
	if(missing(Nrs) || missing(fit))
		stop("Nrs and/or fit missing.")
	if(tolower(type) != "ha" && tolower(type) != "hnull")
		stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
	
	# Get all the ZT values
	ZTstatMatrix <- matrix(0, numMC, 2)
	for(i in 1:numMC)
		ZTstatMatrix[i,] <- ZT.statistics.Hnull.Ha(Nrs, fit, type)
	
	# Pull out z and t and remove NAs
	z <- ZTstatMatrix[,1]
	z <- z[!is.na(z)]
	t <- ZTstatMatrix[,2]
	t <- t[!is.na(t)]
	
	# Get a reference value from the real data
	qAlpha <- qchisq(p=(1-siglev), df=length(fit$pi)-1, ncp=0, lower.tail=TRUE)
	
	# Calculate our pvalues for z and t
	zpval <- (sum(z > qAlpha) + 1)/(length(z) + 1)
	tpval <- (sum(t > qAlpha) + 1)/(length(t) + 1)
	
	return(cbind(zpval, tpval))
}
