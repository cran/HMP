C.alpha.multinomial <-
function(data){
if(missing(data))
stop("data missing.")

T <- T.statistics(data)
K <- ncol(data)
Nreads <- apply(data, 1, sum)/sum(apply(data, 1, sum))

M.alpha <- diag(Nreads)-as.matrix(Nreads) %*% t(as.matrix(Nreads))
g <- sum(diag(M.alpha %*% M.alpha)) / sum(diag(M.alpha))
h <- (K-1)*((sum(diag(M.alpha)))^2) / (sum(diag(M.alpha %*% M.alpha)))

p.value <- 1-pchisq(q=T/g, df=h, ncp=0, lower.tail=TRUE)

GoF.test <- list(T, p.value)
names(GoF.test) <- c("T statistics", "p value")

return(GoF.test)
}
