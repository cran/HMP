Xdc.statistics <-
function(group.data, epsilon=10^(-4)){
fit <- lapply(group.data, function(x, epsilon){
dirmult(x, init=DM.MoM(x)$gamma, epsilon, trace=FALSE)
}, epsilon=epsilon)

logliks <- unlist(lapply(fit, function(x){x$loglik}))
groupData <- NULL

for(i in 1:length(group.data))
groupData <- rbind(groupData, group.data[[i]])

fit.group <- dirmult(groupData, init=DM.MoM(groupData)$gamma, epsilon=epsilon, trace=FALSE)
Xdc <- -2*(fit.group$loglik-sum(logliks))

return(Xdc)
}
