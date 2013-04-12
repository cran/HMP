MC.Xmc.statistics <-
function(Nrs, MC, pi0, group.pi, group.theta, type="ha", siglev=0.05) {
if(missing(Nrs) || missing(MC) || missing(group.theta) || missing(pi0))
stop("Nrs, MC, pi0 and/or group.theta missing.")
if(missing(group.pi) && tolower(type) == "ha")
stop("group.pi missing.")

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

MCC <- as.matrix(seq(1, 1, length=MC))
n.groups <- length(group.theta)
index <- as.matrix(seq(1:n.groups))
group.parameter <- list()
K <- length(pi0)

if(tolower(type) == "hnull"){
for (i in index)
group.parameter[[i]] <- c(pi0, group.theta[i], Nrs[[i]])
}else if(tolower(type) == "ha"){
for (i in index) 
group.parameter[[i]] <- c(group.pi[i,], group.theta[i], Nrs[[i]])
}else{
stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
}

Xmc <- t(t(apply(MCC, 1, function(x) {Xmc.statistics.Hnull.Ha(K, pi0, group.parameter)})))
Xmc_pval <- apply(Xmc, 2, function(t){
q.alpha <- qchisq(p=(1-siglev), df=length(group.theta)*(K-1), ncp=0, lower.tail=TRUE)
sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))
})

return(Xmc_pval)
}
