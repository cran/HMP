rpartCS <-
function(fit) {
	# Pull out split information
	splitNames <- rownames(fit$splits)
	allVars <- colnames(attributes(fit$terms)$factors)  
	
	# Rename splits to fit into data frame
	rownames(fit$splits) <- 1:nrow(fit$splits)
	splits <- data.frame(fit$splits)
	splits$var <- splitNames
	splits$type <- ""
	splits$primary <- ""
	
	# Get the frame
	frame <- as.data.frame(fit$frame)
	frame$var <- as.character(frame$var)
	primeFr <- frame[frame$var != "<leaf>",]
	
	# Go through every node and check competing and surrogaet splits
	index <- 0
	for(i in 1:nrow(primeFr)){
		spltPrimName <- paste("Split", primeFr$yval[i], primeFr$var[i])
		
		# Fill in primary info
		index <- index + 1
		splits$type[index] <- "primary"
		splits$primary[index] <- spltPrimName
		
		# Check for competing splits
		if(primeFr$ncompete[i] > 0){
			for(j in 1:primeFr$ncompete[i]){
				index <- index + 1
				splits$type[index] <- "competing"
				splits$primary[index] <- spltPrimName
			}
		}
		
		# Check for surrogate splits
		if(primeFr$nsurrogate[i] > 0){
			for(j in 1:primeFr$nsurrogate[i]){
				index <- index + 1
				splits$type[index] <- "surrogate"
				splits$primary[index] <- spltPrimName
			}
		}
	}
	
	return(splits)
}
