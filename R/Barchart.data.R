Barchart.data <-
function(data, title="Taxa Proportions"){
if(missing(data))
stop("data missing.")

Data.prop <- t(t(apply(data, 1, function(x){x/sum(x)})))

barplot(Data.prop, col=rainbow(ncol(data)), horiz=TRUE, 
main=title, axisnames=FALSE, font.main=20, font.sub=16)
}
