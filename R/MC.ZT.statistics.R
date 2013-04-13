MC.ZT.statistics <-
function(Nrs, MC, fit, type="ha", siglev=0.05) {
if(missing(Nrs) || missing(MC) || missing(fit))
stop("Nrs, MC and/or fit missing.")

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

MCC <- as.matrix(seq(1, 1, length=MC))
Nrs <- t(t(Nrs))
ZTt <- t(apply(MCC, 1, function(x){ZT.statistics.Hnull.Ha(Nrs, fit, type)}))

tpval <- apply(as.matrix(ZTt[,2]), 2, function(t){
q.alpha <- qchisq(p=(1-siglev), df=length(fit$pi)-1, ncp=0, lower.tail=TRUE)
ret <- sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))
return(ret)
})

zpval <- apply(as.matrix(ZTt[,1]), 2, function(t){
q.alpha <- qchisq(p=(1-siglev), df=length(fit$pi)-1, ncp=0, lower.tail=TRUE)
ret <- sum((t[t!="NaN"]>q.alpha)/(sum(t!="NaN")))
return(ret)
})

return(cbind(zpval, tpval))
}
