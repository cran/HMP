Xdc.statistics.Hnull.Ha <-
function(alphap, Nrs, n.groups, type, est){
group.data.null <- list()
index <- as.matrix(seq(1:n.groups))

if(tolower(type) == "hnull"){
for(x in index)
group.data.null[[x]] <- Dirichlet.multinomial(Nrs[[x]], shape=alphap)
}else if(tolower(type) == "ha"){
for(x in index)
group.data.null[[x]] <- Dirichlet.multinomial(Nrs[[x]], shape=alphap[x,]) 
}else{
stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
}

if(tolower(est) == "mle"){
Xdc <- Xdc.statistics(group.data.null)
}else if(tolower(est) == "mom"){
Xdc <- Xdc.statistics.MoM(group.data.null)
}else{
stop(sprintf("Est '%s' not found. Est must be 'mle' or 'mom'.", as.character(est)))
}

return(Xdc)
}
