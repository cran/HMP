MC.Xoc.statistics <-
function(Nrs, MC, group.alphap, n.groups, type="ha", siglev=0.05) {
if(missing(Nrs) || missing(group.alphap) || missing(n.groups) || missing(MC))
stop("Nrs, group.alphap, n.groups, and/or MC missing.")

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

MCC <- as.matrix(seq(1, 1, length=MC))

Xoc <- t(t(apply(MCC, 1, function(x){Xoc.statistics.Hnull.Ha(Nrs, group.alphap, n.groups, type)})))

Xoc_pval <- apply(Xoc, 2, function(t){
q.alpha <- qchisq(p=(1-siglev), df=(n.groups-1), ncp=0, lower.tail=TRUE)
sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))
})

return(Xoc_pval)
}
