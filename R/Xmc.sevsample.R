Xmc.sevsample <-
function(group.data, pi0){
if(missing(group.data) || missing(pi0))
stop("group.data and/or pi0 missing.")

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

rank.Bj <- sum(pi0>0)-1 
Xmc <- Xmc.statistics(group.parameter.estimated, pi0)
p.value <- 1-pchisq(q=Xmc, df=n.groups*rank.Bj, ncp=0, lower.tail=TRUE)

sevRAD.mean.test <- list(Xmc, p.value)
names(sevRAD.mean.test) <- c("Xmc statistics", "p value")

return(sevRAD.mean.test)
}
