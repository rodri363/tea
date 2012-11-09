#' Estimate a GAM (generalized additive  model) on the given data
#' The function takes one argument, an environment.  The following are
#' the elements expected in the environment
#' @param Data = a data frame containing the variables promised in the formula
#' @param Formula = the formula used to generate fit
#' @return nothing, but update the environment with a new item:
#' Fit = an object of class 'tree', giving the fit.

TEA.gam.est <- function(env){
	Fit <- try(gam(env$Formula,data=env$Data))
	if(inherits(Fit,"try-error")) stop(paste("gam on", env$Formula, "did not work for given data"))
	env$Fit <- Fit
	env$Newdata <- env$Data #set new data to data used for fit; user can modify this
}

#' Draw synthetic values from a GAM fit, using predictive mean matching.
#' The function takes one argument, an environment.  The following are
#' the elements expected in the environment
#' @param Data = a data frame containing the variables promised in the formula
#' @param Formula = the formula used to generate fit
#' @param Fit = an object of class 'tree' giving the fit
#' @return a vector containing the synthetic values

TEA.gam.draw <- function(env){
	lhs  <- all.vars(env$Formula)[1]
	originals <- env$Data[,lhs] #store original non-missing values from data used for FIT
	vwch <- which(!is.na(originals))
	originals <- originals[vwch]
	#predicted from original data non-missing
	opredicted <- predict.gam(env$Fit,newdata=env$Data[vwch,],type="link")
	#predicted from NEW data
	npredicted <- predict.gam(env$Fit,newdata=env$Newdata,type="link")
	#all predicted together
	predicted <- c(opredicted,npredicted)
	if(inherits(predicted,"try-error")) stop("prediction not successful")
	dists <- as.matrix(dist(predicted)) #get distances of fitted valued
	diag(dists) <- Inf #set diagonal to infinity
	dists[is.na(dists)] <- Inf #set NA values to infinity
	#take off rows for original data
	dists <- dists[-(1:length(opredicted)),]
	#take off columns for newdata
	dists <- dists[,-(1:length(npredicted))]
	#find maximal -distance, so minimal distance. diag is inf, so will never be minimal
	donors <- max.col(-dists)
	donvals <- originals[donors]
	ret <- env$Newdata
	ret[,lhs] <- donvals
	return(ret)
}

teagam <- new("apop_model", name="teagam",  
                                estimate_function=TEA.gam.est,
                                draw_function=TEA.gam.draw)

#' Given a model formula and a data frame, fit a multinomial model
#' via the MCMCmnl() function and return a list of items to be used
#' by TEA.predict.MCMCmnl
#' @param env = an environment, containing the following objects:
#' Formula = a formula object
#' Data = a data frame on which to perform modeling
#' @return Nothing, but the following items are added to the environment 'env':
#' Fit: a matrix of posterior parameter draws returned by MCMCmnl
#' Newdata: a copy of Data, which will be used as the default dataset
#' for future draws from the model.

TEA.MCMCmnl.est <- function(env){
	if(is.null(env$debug)) env$debug <- 0
	if(env$debug>0) browser()
	#all variables
	Vvar <- all.vars(env$Formula)
	env$Data <- env$Data[Vvar]
	env$Data <- TEAConformDF(env$Data,env$Data)
	env$Fit <- try(MCMCmnl(env$Formula,data=env$Data))
	if(inherits(env$Fit,"try-error")){
		save(Data,file="err_data.RData",envir=env)
		stop(paste("MCMCmnl() on", deparse(env$Formula), "did not work for given data"))
	}
	env$Newdata <- env$Data #set new data to data used for fit; user can modify this
}

#' Draw posterior predictive values from an MCMCmnl fit.
#' @param env = an environment, containing minimally the following elements:
#' Fit = a matrix of posterior parameter draws returned by MCMCmnl
#' Formula = the formula used to generate Fit
#' Newdata = a data frame, used for making draws
#' Additional optional arguments are:
#' kzero = logical constant.  If TRUE, negative probabilities will be set to 0 and the
#' set of probabilities re-weighted.  If FALSE, errors due to negative probabilities
#' will occur. Default is TRUE.
#' verbose = logical constant.  If TRUE, print more on progress. Default is FALSE.
#' debug = logical constant. If TRUE, enter browser upon function entry.  Default is FALSE.
#' @return a data frame, having drawn values for the LHS of Formula
#' substituted into Newdata.

TEA.MCMCmnl.draw <- function(env){
	if(is.null(env$verbose)) env$verbose <- FALSE
	if(is.null(env$debug)) env$debug <- 0
	if(env$debug>0) browser()

	if(nrow(env$Newdata)==0) stop("Newdata has 0 rows")
	#if no option for negative probilities set, set it
	if(is.null(env$kzero)) env$kzero <- TRUE
	env$Newdata <- TEAConformDF(env$Newdata,env$Data)

	#all.vars breaks when using transform variables e.g. log(x1)
	Vvar <- attr(terms(env$Formula),"term.labels")
	newform <- as.formula(paste("~",paste(Vvar,collapse="+")))
	#Conform levels of new data and model data
	Msub <- model.matrix(newform,env$Newdata)

	#get parameter rows; these are constant across response levels
	#different row for each record?
	#Vrow <- sample(1:nrow(Fit),nrow(Msub),replace=TRUE)
	#same row for all
	Vrow <- sample(1:nrow(env$Fit),1)
	#since we are matching up factor levels, can just use levels() here
	Vlev <- levels(env$Newdata[[all.vars(env$Formula)[1]]])
    Mp <- NULL
	#do prediction for a single response level
	#leave out first level of response
	#which is reference
	for(ldx in 2:length(Vlev)){
		klev <- Vlev[ldx]
		#don't grep, use dimensions
		Vcol <- ((1:ncol(env$Fit))-(ldx-1))%%(length(Vlev)-1)==0
		#params for each row
		Mlev <- env$Fit[Vrow,Vcol]
		Mlev <- matrix(Mlev)
		if(nrow(Msub)==1) Mlev <- t(Mlev) #handle vector beta for a single row
		Vlogits <- Msub %*% Mlev #logits
		Mp <- cbind(Mp,Vlogits)
	}
	#at this point, Mp is a matrix of logits
	#probability of reference category
	Vpk <- 1/(1+rowSums(exp(Mp)))
	#probability for other categories, baselined off reference
	Mp <- Vpk*exp(Mp)
	Mp <- cbind(Vpk,Mp)

    if(sum(ls(env)=="kzero")!=0 && env$kzero){
		Mp[Mp<0] <- 0
		Mp <- Mp/rowSums(Mp)
	}
	Vdraw <- apply(Mp,1,function(Vp) return(sample(Vlev,1,prob=Vp)))

	lhs <- all.vars(env$Formula)[1]
	DFret <- env$Newdata
	DFret[,lhs] <- Vdraw
	if(env$verbose) print(table(DFret[,lhs]))
	return(DFret)
}

mcmc.mnl <- new("apop_model", name="MCMC multinomial",  
                                estimate_function=TEA.MCMCmnl.est, 
                                draw_function=TEA.MCMCmnl.draw)
#' Given a model formula and a data frame, fit a linear regression model
#' via the MCMCregress() function and return a list of items to be used
#' by TEA.predict.MCMCregress
#' @param Formula = a formula object
#' @param Data = a data frame on which to perform modeling
#' @return a list containing the following named items
#' Fit: a matrix of posterior parameter draws returned by MCMCregress
#' Formula: the formula given as an argument
#' Mmod: a model matrix generated by model.matrix(lm(Formula,Data))
TEA.MCMCregress.est <- function(env){
	env$Fit <- try(MCMCregress(env$Formula,data=env$Data))
	if(inherits(env$Fit,"try-error")) stop(paste("MCMCregress() on",env$Formula,"did not work for given data"))
	Fitml <- try(lm(env$Formula, data=env$Data))
	if(inherits(env$Fit,"try-error")) stop(paste("lm() on",Formula,"did not work for given data"))
	#model matrix
	env$Mmod <- model.matrix(Fitml)
	#levels of response
	Vvar <- all.vars(env$Formula)
	flev <- function(var, Data){
		if(is.character(Data[,var])) return(levels(factor(Data[,var])))
		if(is.factor(Data[,var])) return(levels(Data[,var]))
		return(NA)
	}
	env$Newdata <- env$Data #set new data to data used for fit; user can modify this
}

#' Draw posterior predictive values from an MCMCregress fit.
#' Made to work nicely with a return object from TEA.fit.MCMCregress
#' (via do.call() by addition of a "DFsub" element to the list returned by TEA.fit.MCMCregress),
#' but can be used manually.
#' @param Fit = a matrix of posterior parameter draws returned by MCMCregress
#' @param Formula = the formula used to generate Fit
#' @param Mmod = a model matrix, typically generated by model.matrix(),
#' but can be any matrix that has column names conforming to Formula.
#' Used to match levels of predicted data with that of the fitting data.  New levels in
#' predicted data will cause an error; missing levels will accounted for.
#' @param fround = a function with which to round the draws; defaults to floor
#' @return a vector containing the posterior predictive draws
TEA.MCMCregress.draw <- function(env){
	if(is.null(env$verbose)) env$verbose <- FALSE
	if(is.null(env$debug)) env$debug <- FALSE
	if(env$debug>0) browser()
	#set default back-transform and rounding functions to identity
	if(is.null(env$transform)) env$transform <- function(x) return(x)
	if(is.null(env$fround)) env$fround <- function(x) return(x)

	if(nrow(env$Newdata)==0) stop("Newdata has 0 rows")
	#trim off response from formula so model.matrix works
	#can't use all.vars if we want predictors like "log(A)" to work
	#using "term.labels" attribute instead"
	vtermlab <- attr(terms(env$Formula),"term.labels")
	newform <- as.formula(paste("~",paste(vtermlab,collapse="+")))

	lhs <- all.vars(env$Formula)[1]

	#do level check on each character/factor variable
	env$Newdata <- TEAConformDF(env$Newdata,env$Data)
	Msub <- model.matrix(newform,env$Newdata)
	#get parameter rows
	#sample a row for each observation
#	Vrow <- sample(1:nrow(env$Fit),nrow(Msub),replace=TRUE)
	#same row for entire implicate
#	Vrow <- rep(sample(1:nrow(env$Fit),1),nrow(Msub))
	Vrow <- sample(1:nrow(env$Fit),1)
	#betas, -sigma2 column
	Mbeta <- env$Fit[Vrow,-ncol(env$Fit)]
	Mbeta <- matrix(Mbeta)
	if(nrow(Msub)==1) Mbeta <- t(Mbeta) #handle vector beta for a single row
	Vsig <- sqrt(env$Fit[Vrow,ncol(env$Fit)]) #sigma
	Vmu <- Msub %*% Mbeta
	Vdraw <- rnorm(nrow(Msub),Vmu,Vsig)

	DFret <- env$Newdata
	#return reverse-transformed data (in case of "log" type fits)
	DFret[,lhs] <- env$fround(env$transform(Vdraw))
	return(DFret)
}

mcmc.reg <- new("apop_model", name="MCMC regression",  
                                estimate_function=TEA.MCMCregress.est, 
                                draw_function=TEA.MCMCregress.draw)

#' Fit a classification or regression tree in TEA
#' @param env an environment, containing at minimum the following objects
#' env$Formula a formula describing the tree fit
#' env$Data a data set on which to fit the tree
#' returns NULL, but adds the following elements to env
#' env$Fit the tree fit
#' env$Newdata a copy of env$Data, to be used to make draws by default
teatree.est <- function(env){
	if(is.null(env$Formula)) stop("Trees need formulas")
	if(is.null(env$Data)) stop("Trees need data")
	env$Fit <- tree(env$Formula,env$Data)
	env$Newdata <- env$Data
}

#' Make a draw from a tree, given new data
#' @param env an environment, containing at minimum the following objects
#' env$Formula a formula describing the tree fit
#' env$Fit the tree fit
#' env$Newdata data for which draws are desired
#' Can take the following additional objects:
#' env$Method the method of drawing.  One of either "bb" (for Bayesian Bootstrap)
#' or "kde" (for a kernel density estimate on top of the bootstrap).  "kde" is
#' only valid for regression trees.
#' kde still needs implementation
#' returns NULL but adds the following objects to env
#' env$Drawdata the drawn data
teatree.draw <- function(env){
	if(is.null(env$Formula)) stop("Trees need formulas")
	if(is.null(env$Newdata)) stop("Need Newdata to do a draw")
	if(is.null(env$Fit)) stop("Need a tree fit to do a draw")
	if(is.null(env$Method)) env$Method <- "bb" #default to bayes bootstrap
	if(is.null(env$na.rm)) env$na.rm <- TRUE #default to remove NAs from fit
	env$Drawdata <- env$Newdata #initialize drawn data
	klhs <- all.vars(env$Formula)[1] #variable to draw
	#nodes assigned to each record in Newdata
	vwhere <- predict.tree(env$Fit,newdata=env$Newdata,"where")
	#for each node/leaf with values, do a bootstrap or bootstrap+kde
	for(kval in unique(vwhere)){
		vvals <- env$Newdata[vwhere==kval,klhs] #values from leaf (includes NAs)
		if(env$na.rm) vbbvals <- vvals[!is.na(vvals)] #remove NAs before BB
		if(length(vbbvals)>1)
			env$Drawdata[vwhere==kval,klhs] <- TEAbb(vbbvals,klength=length(vvals))
		else
			env$Drawdata[vwhere==kval,klhs] <- vbbvals
	}
	return(NULL)
}

tea.tree <- new("apop_model", name="tree",  
                                estimate_function=teatree.est,
                                draw_function=teatree.draw)
