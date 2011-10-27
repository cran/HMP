Barchart.data <-
function(data, level){
	if(missing(data)){
		stop("data missing")
	}else if(missing(level)){
		stop("level missing")
	}

	K=dim(data)[2]
	Data.prop=t(apply(data,1,function(x){x/sum(x)}))	

	barplot(t(Data.prop), col = rainbow(K), horiz = TRUE, main = paste("Taxa Proportions at ",level, " level"), sub = "Ranked Taxa Proportions", axisnames = FALSE,font.main=20,font.sub=16)
}

