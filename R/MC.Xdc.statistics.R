MC.Xdc.statistics <-
function(Nrs, MC, alphap, n.groups, type="ha", siglev=0.05, est="mom") {
if(missing(Nrs) || missing(alphap) || missing(n.groups) || missing(MC))
stop("Nrs, alphap, n.groups and/or MC missing.")

MCC <- as.matrix(seq(1, 1, length=MC))

if(tolower(type) == "hnull"){
K <- length(alphap)
}else if(tolower(type) == "ha"){
K <- ncol(alphap)
}else{
stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
}

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

Xdc <- t(t(apply(MCC, 1, function(x){Xdc.statistics.Hnull.Ha(alphap, Nrs, n.groups, type, est)})))

Xdc_pval <- apply(Xdc, 2, function(t){
q.alpha <- qchisq(p=(1-siglev), df=(n.groups-1)*K, ncp=0, lower.tail=TRUE)
sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))
})

return(Xdc_pval)
}
