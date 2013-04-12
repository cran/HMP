Xmcupo.sevsample <-
function(group.data, K){
if(missing(group.data) || missing(K))
stop("group.data and/or K missing.")

n.groups <- length(group.data)
index <- as.matrix(seq(1:n.groups))
group.parameter.estimated <- list()

for(x in index){
data <- group.data[[x]]
nreads.data <- as.matrix(apply(data, 1, sum))

pi.MoM <- apply(data, 2, sum)/sum(apply(data, 2, sum))
theta.MoM <- weirMoM(data)
group.parameter.estimated[[x]] <- c(pi.MoM, theta.MoM, t(nreads.data))
}

Xmcupo <- Xmcupo.statistics(group.parameter.estimated, K)
p.value <- 1-pchisq(q=Xmcupo, df=(n.groups-1)*(K-1), ncp=0, lower.tail=TRUE)

sevRAD.mean.test.upo <- list(Xmcupo, p.value)
names(sevRAD.mean.test.upo) <- c("Xmcupo statistics", "p value")

return(sevRAD.mean.test.upo)
}
