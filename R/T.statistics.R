T.statistics <-
function(data){
T <- sum((1/apply(data, 2, sum)) * 
apply((data-(1/sum(data))*as.matrix(apply(data, 1, sum)) %*% 
t(as.matrix(apply(data, 2, sum))))^2, 2, sum))

return(T)
}
