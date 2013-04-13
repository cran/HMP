Xmc.statistics <-
function(group.parameter, pi0){
K <- length(pi0)
n.groups <- length(group.parameter)
index <- as.matrix(seq(1:n.groups))

Xscg <- apply(index, 1, function(x){
pi <- group.parameter[[x]][1:K]
theta <- group.parameter[[x]][K+1]
P <- length(group.parameter[[x]])
nreads.data <- group.parameter[[x]][(K+2):P]
Bj <- ((theta*(sum(nreads.data^2)-sum(nreads.data))+sum(nreads.data)) / 
(sum(nreads.data))^2) * (diag(as.vector(pi0))-pi0 %*% t(pi0))
Xsc <- t(pi-pi0) %*% ginv(Bj, tol=sqrt(.Machine$double.eps)) %*% (pi-pi0)
return(Xsc)
})

return(sum(Xscg))
}
