kullbackLeiber <-
function(data, plot=TRUE, parallel=FALSE, cores=3){
	warning("This function has been spellchecked.  Please use 'Kullback.Leibler' instead.")
	kl <- Kullback.Leibler(data, plot, parallel, cores)
	return(kl)
}
