gaPlot <-
function(evalSumm){
	plot(evalSumm[,4], type="l", ylab="Score", ylim=c(0, 1), lwd=2, main="Eval Scores by Iteration", xlab="Iteration")
	lines(evalSumm[,6], col="red", lwd=2)
	lines(evalSumm[,1], col="blue", lwd=2)
	legend("topleft", colnames(evalSumm)[c(4, 6, 1)], pch=16, col=c("black", "red", "blue"))
}
