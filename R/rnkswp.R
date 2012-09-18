
# \key rankSwap/seed The random number generator seed for the rank swapping setup.

# \key{rankSwap/swap range} proportion of ranks to use for swapping interval, that is
# if current rank is r, swap possible from r+1 to r+floor(swaprange*length(x))
#
# default = 0.5
#
# \key{rankSwap/max change} maximal absolute change in value of x allowed.
# That is, if the swap value for $x_i$ is $y$, if $|y - x_i| >$ maxchange,
# then the swap is rejected
#
# default = 1


#' Swap items in a real/numeric vector by ranks, with
#' constraint on maximal distance of change in value
#' @param x a real/numeric vector to be swapped
#' @param swaprange proportion of ranks to use for swapping interval, that is
#' if current rank is r, swap possible from r+1 to r+floor(swaprange*length(x))
#' @param maxchange maximal absolute change in value of x allowed.
#' That is, if the swap value for xi is y, if |y - xi| > maxchange,
#' then the swap is rejected
#' @return the swapped vector
RankSwapConstrained <- function(x=NULL, table=NULL, column=NULL, swap_range=0.5, max_change=1, seed=31337){
	key1 <- "rankSwap"
	potentialseed <-PEPGetKey(key1,"seed")
	potentialswaprange <-PEPGetKey(key1,"swap range")
	potentialmaxchange <-PEPGetKey(key1,"max change")
	if(!is.null(potentialseed)) seed <- potentialseed
	if(!is.null(potentialmaxrange)) seed <- potentialmaxrange
	if(!is.null(potentialswaprange)) seed <- potentialswaprange

	#we accept either a vector x or a db table column
	if(is.null(x)) {
		if(is.null(column)) column <- PEPGetKey(key1, "column")
		if(is.null(table)) table <- PEPGetKey(key1, "table")
		if(is.null(table)) table <- pepenv$active_tab
		if(is.null(column)) stop("I need to know from which colum of the table I should draw")
		if(is.null(table)) stop("I need to know from which table I should draw")
		x <- teaTable(table, cols=column)
	}

	ox <- order(x)
	lx <- length(x)
	cutoffsx <- c(which(diff(x[ox])>0),lx)
	lcutx <- length(cutoffsx)
	nrnk <- .C("rnkswpConstr",as.double(x[ox]),
				as.integer(ox),
				as.integer(lx),as.double(swap_range),
				as.integer(cutoffsx),as.integer(lcutx),
				as.integer(max_change),
				as.integer(seed))[[2]];
	if(pepenv$verbosity) print(nrnk)
	x[ox] <- x[nrnk];
	return(x)
}

# There used to be a few other alternate functions for rank swapping, 
# on the R side and in src/rnkswp/rnkswp.c. To retrieve:
# git checkout 851a4fac
