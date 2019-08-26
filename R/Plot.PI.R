Plot.PI <-
function(estPi, errorBars=TRUE, logScale=FALSE, main="PI Vector", ylab="Fractional Abundance"){
	if(missing(estPi))
		stop("estPi is missing.")
	
	# Move title to the middle
	ggplot2::theme_update(plot.title=ggplot2::element_text(hjust=0.5))
	
	# Make the base plot
	piPlot <- ggplot2::ggplot(estPi$params, ggplot2::aes_string(y="PI", x="Taxa", colour="Group")) +
			ggplot2::geom_point() + 
			ggplot2::theme(legend.position = "top", text=ggplot2::element_text(size=15)) +
			ggplot2::labs(title=main, y=ylab, x="") +
			ggplot2::theme(axis.text.x=ggplot2::element_text(hjust=1, angle=45, size=10))
	
	# Add error bars
	if(errorBars){
		piPlot <- piPlot + ggplot2::geom_errorbar(ggplot2::aes_string(ymax="Upper", ymin="Lower"))
	}else{
		piPlot <- piPlot + ggplot2::geom_line(ggplot2::aes_string(group="Group"))
	}
	
	# Do log scaling
	if(logScale)
		piPlot <- piPlot + ggplot2::scale_y_log10()
	
	if(logScale)
		piPlot <- piPlot + ggplot2::labs(y=paste(ylab, "(Logged)"))
	
	print(piPlot)
}
