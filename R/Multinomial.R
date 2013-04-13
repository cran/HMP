Multinomial <-
function(Nrs, probs){
if(missing(Nrs) || missing(probs))
stop("Nrs and/or probs missing.")

for(n in Nrs){
if(all(n!=n[1])){
warning("Unequal number of reads across samples.")
break
}
}

Nrs <- t(t(Nrs))
Sample.counts <- t(apply(Nrs, 1, function(x){rmultinom(n=1, size=x, prob=probs)}))
data <- Sample.counts[, colSums(Sample.counts) != 0]

return(data)
}
