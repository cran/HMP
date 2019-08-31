### R code from vignette source 'DMRP_Paper.Rnw'

###################################################
### code chunk number 1: initializing
###################################################

library(HMP)

data(dmrp_data)
data(dmrp_covars)
	


###################################################
### code chunk number 2: figure1 (eval = FALSE)
###################################################
## 
## # Set splitting parameters for DM-Rpart (see ??DM-Rpart for details)
## minBucket <- 6
## minSplit <- 18
## 
## # Set the number of cross validations
## # 20 means the model will run 20 times, each time holding 5% of the data out
## numCV <- 20	
## 
## # Run the DM-RPart function with a seed set
## set.seed(2019)
## DMRPResults <- DM.Rpart.CV(dmrp_data, dmrp_covars, plot=FALSE, minsplit=minSplit, 
## 		minbucket=minBucket, numCV=numCV)
## 
## # Pull out and plot the best tree
## bestTree <- DMRPResults$bestTree
## rpart.plot::rpart.plot(bestTree, type=2, extra=101, box.palette=NA, branch.lty=3, 
## 		shadow.col="gray", nn=FALSE)
## 


###################################################
### code chunk number 3: figure2 (eval = FALSE)
###################################################
## 
## # Split the data by terminal nodes
## nodeNums <- bestTree$frame$yval[bestTree$frame$var == "<leaf>"]
## nodeList <- split(dmrp_data, f=bestTree$where)
## names(nodeList) <- paste("Node", nodeNums)
## 
## # Get the PI for each terminal node
## myEst <- Est.PI(nodeList)
## myPI <- myEst$MLE$params
## 
## # Plot the PI for each terminal node
## myColr <- rainbow(ncol(dmrp_data))
## lattice::barchart(PI ~ Group, data=myPI, groups=Taxa, stack=TRUE, col=myColr, 
## 		ylab="Fractional Abundance", xlab="Terminal Node", 
## 		auto.key=list(space="top", columns=3, cex=.65, rectangles=FALSE, 
## 				col=myColr, title="", cex.title=1))
## 


