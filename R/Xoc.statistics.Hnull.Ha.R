Xoc.statistics.Hnull.Ha <-
function(Nrs, group.alphap, n.groups, type){
group.data.null <- list()
index <- as.matrix(seq(1:n.groups))

if(tolower(type) == "hnull"){
for(x in index)
group.data.null[[x]] <- Dirichlet.multinomial(Nrs[[x]], shape=group.alphap)
}else if(tolower(type) == "ha"){
for(x in index)
group.data.null[[x]] <- data <- Dirichlet.multinomial(Nrs[[x]], shape=group.alphap[x,]) 
}else{
stop(sprintf("Type '%s' not found. Type must be 'ha' for power or 'hnull' for size.\n", as.character(type)))
}

Xoc <- Xoc.statistics(group.data.null)

return(Xoc)
}
