library(tea)

read_spec("spec")
DF <- dbGetQuery(pepenv$con,"select * from edit")
vedvar <- dbGetQuery(pepenv$con,"select name from variables")$name
Lalt <- sapply(1:nrow(DF),function(kr)
 return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",pepenv$con)))

#distance function
#vrec a named element from a data frame
#vrec a named weight vector (same names as DFalt)
fdist <- function(vrec,DFalt,vwgt){
	if(is.null(DFalt)) return(NULL)
	vnames <- names(DFalt)
	if(!all(vnames %in% names(vrec))) stop("Don't have all the variables needed")
	vrec <- vrec[vnames] #put record in right order
	vwgt <- vwgt[vnames] #put weights in right order
	vrec <- as.character(as.matrix(vrec))
	Malt <- as.matrix(DFalt)
	Mtf <- t(apply(Malt,1,function(x) return(Vectorize(identical)(x,vrec))))
	if(nrow(Mtf)==1) Mtf <- t(Mtf)
	return((1-Mtf)%*%vwgt)
}

#function to get minimal distance match
fmin <- function(x,vwgt){
	Mdist <- fdist(DF[x,],Lalt[[x]],vwgt)
	print(Mdist)
	return(Lalt[[x]][Mdist==min(Mdist),,drop=FALSE])
}
vwgt <- c(age=1,sex=1,schl=1)
ll1 <- sapply(1:nrow(DF),fmin,vwgt)
vwgt <- c(age=2,sex=2,schl=5)
ll2 <- sapply(1:nrow(DF),fmin,vwgt)
print(identical(ll1,ll2))
