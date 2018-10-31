Est.PI <-
function(group.data, conf=.95){
	if(missing(group.data))
		stop("group.data is missing.")
	
	# Check the number of groups
	numGroups <- length(group.data)
	
	# Make sure we have the same columns
	taxaCounts <- sapply(group.data, ncol)
	numTaxa	<- taxaCounts[1]
	if(any(taxaCounts != numTaxa)){
		warning("Group columns do not match, running formatDataSets.")
		group.data <- formatDataSets(group.data)
	}
	
	# Make sure we have group names
	if(is.null(names(group.data))){
		grpNames <- paste("Data Set", 1:numGroups)
	}else{
		grpNames <- names(group.data)
	}
	
	# Calculate the pi and error bars for each group
	allParamsMLE <- data.frame(matrix(0, 0, 6))
	allParamsMOM <- data.frame(matrix(0, 0, 6))
	thetaMLE <- data.frame(matrix(0, numGroups, 3))
	thetaMOM <- data.frame(matrix(0, numGroups, 3))
	for(i in 1:numGroups){
		tempData <- group.data[[i]]
		
		# Check the data has samples
		numSub <- nrow(tempData)
		if(numSub < 1)
			stop("At least one data set in group.data is empty")
		
		tempParam1 <- data.frame(matrix(0, ncol(tempData), 6))
		tempParam2 <- data.frame(matrix(0, ncol(tempData), 6))
		
		tempParam2[,2] <- grpNames[i]
		tempParam1[,2] <- grpNames[i]
		
		# Check for taxa with 0 column sums (add 1 to everything if this happens)
		badTaxa <- which(colSums(tempData) == 0)
		if(length(badTaxa) != 0)
			tempData <- tempData + 1
		
		# Handle having 1 sample
		if(numSub == 1){
			tempParam1[,1] <- colnames(tempData)
			tempParam1[,3] <- unlist(tempData[1,]/sum(tempData))
			tempParam1[,4] <- NA
			tempParam1[,5] <- tempParam1[,3]
			tempParam1[,6] <- tempParam1[,3]
			tempParam1 <- tempParam1[order(tempParam1[,1]),]
			
			tempTheta1 <- c(0, NA)
			
			tempParam2 <- tempParam1
			tempTheta2 <- tempTheta1
		}else{	
			# Get the MoM and MLE for every taxa
			fsum <- dirmult::dirmult.summary(tempData, dirmult::dirmult(tempData, trace=FALSE))
			tempTheta <- fsum[nrow(fsum),]
			fsum <- fsum[-nrow(fsum),] 
			
			# Turn the summary into a data frame we can plot from
			tempParam1[,1] <- rownames(fsum)
			tempParam1[,3] <- fsum$MLE
			tempParam1[,4] <- fsum$se.MLE
			tempTheta1 <- tempTheta[,2:3]
			
			tempParam2[,1] <- rownames(fsum)
			tempParam2[,3] <- fsum$MoM
			tempParam2[,4] <- fsum$se.MOM
			tempTheta2 <- tempTheta[,4:5]
			
			# Calc Upper and Lower bounds for CI
			minSubj <- min(sapply(group.data, function(x) nrow(x)))
			if(minSubj < 30){
				val <- stats::qt(0.5 + conf *0.5, df=minSubj-1)
			}else{
				val <- stats::qnorm(0.5 + conf*0.5)
			}
			
			tempParam1[,5] <- tempParam1[,3] + val*tempParam1[,4]
			tempParam1[,6] <- tempParam1[,3] - val*tempParam1[,4]
			
			tempParam2[,5] <- tempParam2[,3] + val*tempParam2[,4]
			tempParam2[,6] <- tempParam2[,3] - val*tempParam2[,4]
		}
		
		# Save outside of loop
		allParamsMLE <- rbind(allParamsMLE, tempParam1)
		thetaMLE[i,] <- c(grpNames[i], tempTheta1)
		
		allParamsMOM <- rbind(allParamsMOM, tempParam2)
		thetaMOM[i,] <- c(grpNames[i], tempTheta2)
	}
	colnames(allParamsMLE) <- c("Taxa", "Group", "PI", "SE", "Upper", "Lower")
	colnames(thetaMLE) <- c("Group", colnames(tempTheta1))
	colnames(allParamsMOM) <- c("Taxa", "Group", "PI", "SE", "Upper", "Lower")
	colnames(thetaMOM) <- c("Group", colnames(tempTheta2))
	
	# Make sure none of our error bars go over 100 or below 0
	allParamsMLE$Upper <- ifelse(allParamsMLE$Upper > 1, 1, allParamsMLE$Upper)
	allParamsMLE$Lower <- ifelse(allParamsMLE$Lower < 0, 0, allParamsMLE$Lower)
	allParamsMOM$Upper <- ifelse(allParamsMOM$Upper > 1, 1, allParamsMOM$Upper)
	allParamsMOM$Lower <- ifelse(allParamsMOM$Lower < 0, 0, allParamsMOM$Lower)
	
	# Factor the data so it stays in the right order
	allParamsMLE$Group <- factor(allParamsMLE$Group, levels=grpNames)
	allParamsMLE$Taxa <- factor(allParamsMLE$Taxa, levels=unique(colnames(group.data[[1]])))
	allParamsMOM$Group <- factor(allParamsMOM$Group, levels=grpNames)
	allParamsMOM$Taxa <- factor(allParamsMOM$Taxa, levels=unique(colnames(group.data[[1]])))
	
	MLE <- list(params=allParamsMLE, theta=thetaMLE)
	MOM <- list(params=allParamsMOM, theta=thetaMOM)
	
	return(list(MLE=MLE, MOM=MOM))
}
