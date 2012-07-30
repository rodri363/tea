library(tea)

read_spec("spec")
DF <- dbGetQuery(con,"select * from edit")
Lalt <- sapply(1:nrow(DF),function(kr)
 return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",con)))

#vrec a named element from a data frame
#vrec a named weight vector (same names as DFalt)
fdist <- function(vrec,DFalt,vwgt){
	if(is.null(DFalt)) return(NULL)
	print(DFalt)
	vnames <- names(DFalt)
	if(!all(vnames %in% names(vrec))) stop("Don't have all the variables needed")
	vrec <- vrec[vnames] #put record in right order
	vwgt <- vwgt[vnames] #put weights in right order
	vrec <- as.character(as.matrix(vrec))
	Malt <- as.matrix(DFalt)
	Mtf <- t(apply(Malt,1,function(x) return(Vectorize(identical)(x,vrec))))
	if(nrow(Mtf)==1) Mtf <- t(Mtf)
	print(Mtf)
	return(Mtf%*%vwgt)
}

vwgt <- c(age=1,sex=1,schl=1)
u <- fdist(DF[3,],Lalt[[3]],vwgt)
fmin <- function(x){
	Mdist <- fdist(DF[x,],Lalt[[x]],vwgt)
	return(Lalt[[x]][Mdist==min(Mdist),,drop=FALSE])
}
u <- sapply(1:nrow(DF),fmin)
