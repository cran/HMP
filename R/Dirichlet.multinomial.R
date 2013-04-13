Dirichlet.multinomial <-
function(Nrs, shape){
if(missing(Nrs) || missing(shape))
stop("Nrs and/or shape missing.")

if(Nrs <= 0 || shape <= 0)
stop("Nrs and shape must be positive.")

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

Nrs <- t(t(Nrs))
Sample.counts <- t(apply(Nrs, 1, function(x){rmultinom(n=1, size=x, prob=rdirichlet(1, shape))}))

return(Sample.counts)
}
