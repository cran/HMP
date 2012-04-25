Barchart.data <-
function(data, taxaLevel="Genus"){
	if(missing(data)){
		stop("data missing")
	}

	K <- ncol(data)
	Data.prop <- t(apply(data,1,function(x){x/sum(x)}))	

	barplot(t(Data.prop), col = rainbow(K), horiz = TRUE, main = paste("Taxa Proportions at", taxaLevel, "level"), sub = "Ranked Taxa Proportions", axisnames = FALSE, font.main = 20, font.sub = 16)
}

