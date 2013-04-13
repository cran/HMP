loglikDM <-
function(data, alphap){
if(missing(data) || missing(alphap))
stop("data and/or alphap missing.")

data <- data[,colSums(data)!=0]
alphap <- alphap[alphap!=0]
Ns <- nrow(data)
K <- ncol(data)

ll <- sum(lgamma(apply(data, 1, sum)+1)+lgamma(sum(alphap)) -
lgamma(sum(alphap)+apply(data, 1, sum))) + 
sum(apply(lgamma(matrix(alphap, nrow=Ns, ncol=K, byrow=TRUE)+data)-lgamma(data+1) - 
lgamma(matrix(alphap, nrow=Ns, ncol=K, byrow=TRUE)), 1, sum))

return(ll)
}
