weirMoM <-
function(data, se=FALSE){
if(missing(data))
stop("data missing.")

K <- ncol(data)
J <- nrow(data)

colSumsdata <- apply(data, 2, function(z){sum(as.numeric(z))})
rowSumsdata <- apply(data, 1, function(z){sum(as.numeric(z))})
totalsample <- sum(apply(data, 2, function(z){sum(as.numeric(z))}))

MoM <- colSumsdata/totalsample
Sn <- rowSumsdata

MSP <- (J-1)^(-1) * sum(rowSums((data/rowSumsdata - matrix(rep(MoM, J), J, K, byrow=T))^2) * Sn)
MSG <- (totalsample-J)^(-1) * sum(rowSums(data/rowSumsdata * (1-data/rowSumsdata)) * Sn)
nc <- 1/(J-1) * (sum(Sn)-sum(Sn^2)/sum(Sn))
MoM.wh <- (MSP-MSG)/(MSP+(nc-1)*MSG)

if(se){
std.er <- sqrt(2 * (1-MoM.wh)^2/(J-1) * ((1+(nc-1) * MoM.wh)/nc)^2)
return(list(theta=MoM.wh, se=std.er))
}else{
return(MoM.wh)
}
}
