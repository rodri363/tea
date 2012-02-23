#' Estimate a GAM (generalized additive  model) on the given data
#' The function takes one argument, an environment.  The following are
#' the elements expected in the environment
#' @param Data = a data frame containing the variables promised in the formula
#' @param Formula = the formula used to generate fit
#' @return nothing, but update the environment with a new item:
#' Fit = an object of class 'tree', giving the fit.

TEA.gam.est <- function(env){
	Fit <- try(gam(env$Formula,data=env$Data))
	if(inherits(Fit,"try-error")) stop(paste("gam on", Formula, "did not work for given data"))
	env$Fit <- Fit
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
	originals <- env$Data[,lhs] #store original values
	predicted <- predict.gam(env$Fit,newdata=env$Data,type="link")
	if(inherits(predicted,"try-error")) stop("prediction not successful")
	dists <- as.matrix(dist(predicted)) #get distances of fitted valued
	diag(dists) <- Inf #set diagonal to infinity
	dists[is.na(dists)] <- Inf #set NA values to infinity
	#find maximal -distance, so minimal distance. diag is inf, so will never be minimal
	donors <- max.col(-dists)
	donvals <- originals[donors]
	ret <- env$Data
	ret[,lhs] <- donvals
	return(ret)
}

teagam <- new("apop_model", name="teagam",  
                                estimate_function=TEA.gam.est,
                                draw_function=TEA.gam.draw)

#' Given a multinom fit (either from multinom or stepAIC),
#' create maximum likelihood-based synthetic data
#' using (possibly) consistency rules.  To use consistency rules,
#' the data frame must have all variables used by the
#' consistency checking system
#' @param multinom.fit = a model fit as returned by the multinom() function
#' @param data = a data frame.  The data frame must contain all the variables
#' promised by multinom.fit.  The data frame need not contain the same number of
#' observations as the data used in the original fit.
#' @param edit.vars = A character vector containing the variables used in the
#' consistency checking system.  Defaults to NULL, which implies no checking
#' @param db = A string giving the database used for the consistency checking system.
#' Defaults to NULL, which implies no checking.
#' @param niter = Integer giving the total iterations allow for the consistency checking system.  Defaults to 10.
#' @param rescale = Logical indicating whether to 'zero-out' probabilities associated
#' with inconsistent draws, and rescale the remaining probabilities to sum to 1.  Default is FALSE.
#'
#' @return a data frame of the same dimensions as data, with synthetic values
#' inserted for the response variable.

PEP.multinom.syn.ml <- function(multinom.fit,data,edit.vars=NULL,db=NULL,niter=10,rescale=FALSE){
	lhs  <- all.vars(multinom.fit$call$formula)[1]
	print(multinom.fit$call$formula)
	levs <- multinom.fit$lev
	originals <- data[,lhs] #store original values
	probs <- as.matrix(predict(multinom.fit,data,"probs")) #draw new vals based on fit
	#if only 2 levels, add back in dropped level probs
	if(ncol(probs)==1) probs <- cbind(1-rowSums(probs),probs)
	#return NA for any bad draws
	f.tmp <- function(x){
	    xtry <- try(sample.int(length(x),1,prob=x),silent=TRUE)
		if(inherits(xtry,"try-error")) xtry <- NA
		return(xtry)
	}
	probIdx <- apply(probs,1,function(x) return(f.tmp(x)))
	predicted <- levs[probIdx]
	data[,lhs] <- predicted
	if(is.null(edit.vars)){
		return(data)
	}else{
		if(!all(edit.vars %in% names(data))){
			stop(paste("Couldn't find",
					paste(setdiff(edit.vars,names(data)),collapse=","),
					"in the data!  Can't do consistency checking. Here are the data columns:",
					paste(names(data),collapse=" , ")))
		}
		if(!is.null(db)) con <- dbConnect(dbDriver("SQLite"),db)
		if(is.null(db)) stop("Can't do consistency without a database connection")
		nfail <- 1
		iter <- 1
		edit.fail <- !logical(nrow(data))
		edit.vals <- mapply(as.matrix,data[,edit.vars])
		while(nfail > 0 & iter<=niter){
			#method: check values for consistency
			#for any row that fails
				#set the row/donor entry of the dists matrix to -inf
				#go again
			edit.sub <- as.logical(apply(edit.vals[edit.fail,,drop=FALSE],1,CheckConsistency,
					vars=edit.vars,what_you_want="passfail",con=con))
			edit.fail[edit.fail] <- edit.sub #this should work, really!
			#reset values for bad records
			data[edit.fail,lhs] <- originals[edit.fail]
			nfail <- sum(edit.fail)
			#redraw new values
			probs <- as.matrix(predict(multinom.fit,data,"probs"))
			if(ncol(probs)==1) probs <- cbind(1-rowSums(probs),probs)
			#zero out previously bad draws the rescale probabilities?
			#note this is done for all rows, so make sure to use edit.fail
			#to select proper rows
			#if only 1 column, don't rescale
			if(rescale & ncol(probs)>1){
				#print("Re-scaling probabilities")
				probs[matrix(c(1:nrow(probs),probIdx),nrow=nrow(probs),byrow=FALSE)] <- 0
				probs <- probs/rowSums(probs)
			}
			probIdx <- apply(probs,1,function(x) return(f.tmp(x)))
			predicted <- levs[probIdx]
			#replaced failed records again
			data[edit.fail,lhs] <- predicted[edit.fail]
			iter <- iter+1
		}
		print(paste("Out of",nrow(data),"records there were:"))
		print(paste(nfail,"inconsistent draws"))
		#put back the remaining inconsistent records to original values
		data[edit.fail,lhs] <- originals[edit.fail]
		if(!is.null(con)) dbDisconnect(con)
		return(data)
	}
}

flev <- function(var, Data){
    if(is.character(Data[,var])) return(levels(factor(Data[,var])))
    if(is.factor(Data[,var])) return(levels(Data[,var]))
    return(NA)
}


# Set up prior: mvn
# set up likelihood: multinomial
# use apop_update to get 

# mvn <- get_C_model("apop_multivariate_normal")
# data <- apop_data_from_data_frame(...)
# est <- estimateRapopModel(data, mvn)


#' Given a model formula and a data frame, fit a multinomial model
#' via the MCMCmnl() function and return a list of items to be used
#' by TEA.predict.MCMCmnl
#' @param Formula = a formula object
#' @param Data = a data frame on which to perform modeling
#' @return a list containing the following named items
#' Fit: a matrix of posterior parameter draws returned by MCMCmnl
#' Formula: the formula given as an argument
#' Mmod: a model matrix generated by model.matrix(multinom(Formula,Data))
#' Llev: the levels/unique values for every factor/character variable referenced in Formula

#TEA.fit.MCMCmnl <- function(Formula,Data){
TEA.fit.MCMCmnl <- function(env){
	Fit <- try(MCMCmnl(env$Formula,data=env$Data))
	if(inherits(Fit,"try-error")) stop(paste("MCMCmnl() on", Formula, "did not work for given data"))
    attach(env)
	Mmod <- model.matrix(Formula,Data)
	#levels of response
	Vvar <- all.vars(Formula)
    Llev <- sapply(Vvar,flev, Data)
    detach(env)
    assign("Llev", Llev, envir=env)
    assign("Fit", Fit, envir=env)
    assign("Mmod", Mmod, envir=env)
}

TEA.draw.mcmc <- function(env){
	return(sample(1:nrow(Fit),1))
}

mcmc.mod <- new("apop_model", name="MCMC MNL",  
                                estimate_function=TEA.fit.MCMCmnl, 
                                draw_function=TEA.draw.mcmc)

#' Draw posterior predictive values from an MCMCmnl fit.
#' Made to work nicely with a return object from TEA.fit.MCMCmnl
#' (via do.call() by addition of a "DFsub" element to the list returned by TEA.fit.MCMCnl),
#' but can be used manually.
#' @param Fit = a matrix of posterior parameter draws returned by MCMCmnl
#' @param Formula = the formula used to generate Fit
#' @param Mmod = a model matrix, typically generated by model.matrix(),
#' but can be any matrix that has column names conforming to Formula.
#' @param Llev = the levels/unique values for every factor/character variable referenced in Formula.
#' Used to match levels of predicted data with that of the fitting data.  New levels in
#' predicted data will cause an error; missing levels will accounted for.
#' @param kzero = logical constant.  If TRUE, negative probabilities will be set to 0 and the set of probabilities
#' re-weighted.  If FALSE, errors due to negative probabilities will occur.
#' @return a vector containing the posterior predictive draws

#TEA.predict.MCMCmnl <- function(Fit,Formula,Mmod,Llev,DFsub,kzero=TRUE){
TEA.logitish.est <- function(env){
#Requires a parameterModel and data
    attach(env)
    attach(env$parameterModel$env) #hack. What gets used?
    flev <- function(var){
        Vret <- data[,var]
        Lret <- list(Vret)
        names(Lret) <- var
        if(is.character(env$data[,var])) Vsub <- levels(factor(env$data[,var]))
        if(is.factor(env$data[,var])) Vsub <- levels(env$data[,var])
        #if a numeric var, just return values
        else return(Lret)
        #see if there are new levels
        Vvar <- Llev[[var]]
        if(is.null(Vvar)) stop("Couldn't find variable",var,"in levels list")
        if(length(setdiff(Vsub,Vvar))>0) stop(paste("New levels found for",var,"in prediction data set"))
        #add missing levels
        if(length(setdiff(Vvar,Vsub))>0){
            Vret <- factor(env$data[,var],levels=c(levels(factor(env$data[,var])),setdiff(Vvar,Vsub)))
            Lret <- list(Vret)
            names(Lret) <- var
            return(Lret)
        }
        else return(Lret)
    }
	#check levels of subset versus model
	#new levels in subset means error
	#missing levels means add to levels
	env$data <- as.data.frame(lapply(all.vars(Formula),flev))
	Msub <- TEAConformMatrix(model.matrix(Formula,env$data),Mmod)
	#get parameter rows; these are constant across response levels
	#Vrow <- sample(1:nrow(Fit),nrow(Msub),replace=TRUE)
	#same row for all!
	Vrow <- rep(RapopModelDraw(env$parameterModel),nrow(Msub))
	#do prediction for a single response level
	#leave out first level of response
	Vlev <- Llev[[all.vars(Formula)[1]]]
    Mp <- NULL
	for(ldx in 2:length(Vlev)){
		klev <- Vlev[ldx]
		Vcol <- grep(paste("[.]",klev,sep=""),colnames(Fit))
		#params for each row
		Mlev <- Fit[Vrow,Vcol]
		Vlogits <- diag(Msub %*% t(Mlev)) #logits
		Vp <- exp(Vlogits)/(1+exp(Vlogits)) #probs for this outcome
		Mp <- cbind(Mp,Vp) #append to prob matrix
	}
	Mp <- cbind(1-rowSums(Mp),Mp) #complete prob matrix
    if(sum(ls(env)=="kzero")!=0 && env$kzero){
		Mp[Mp<0] <- 0
		Mp <- Mp/rowSums(Mp)
	}
    detach(env$parameterModel$env)
    detach(env)
    env$Mp <- Mp
    env$Vlev <- Vlev
}

TEA.logitish.draw <- function(env){
	Vdraw <- apply(env$Mp,1,function(Vp) return(sample(env$Vlev,1,prob=Vp)))
	return(Vdraw)
}

logitish <- new("apop_model", name="logitish",  
                                estimate_function=TEA.logitish.est,
                                draw_function=TEA.logitish.draw)

#' Given a model formula and a data frame, fit a linear regression model
#' via the MCMCregress() function and return a list of items to be used
#' by TEA.predict.MCMCregress
#' @param Formula = a formula object
#' @param Data = a data frame on which to perform modeling
#' @return a list containing the following named items
#' Fit: a matrix of posterior parameter draws returned by MCMCregress
#' Formula: the formula given as an argument
#' Mmod: a model matrix generated by model.matrix(lm(Formula,Data))
#' Llev: the levels/unique values for every factor/character variable referenced in Formula
TEA.fit.MCMCregress <- function(env){
    attach(env)
	Fit <- try(MCMCregress(Formula,data=Data))
	if(inherits(Fit,"try-error")) stop(paste("MCMCregress() on",Formula,"did not work for given data"))
	Fitml <- try(lm(Formula, data=Data))
	if(inherits(Fit,"try-error")) stop(paste("lm() on",Formula,"did not work for given data"))
	Mmod <- model.matrix(Fitml)
	#levels of response
	Vvar <- all.vars(Formula)
	Llev <- sapply(Vvar,flev, Data)
    detach(env)
    env$Fit<-Fit
    env$Mmod<-Mmod
    env$Llev<-Llev
}

#mcmc.reg <- new("apop_model", name="MCMC regression",  
#                                estimate_function=TEA.fit.MCMCregress, 
#                                draw_function=TEA.draw.mcmc)

#' Draw posterior predictive values from an MCMCregress fit.
#' Made to work nicely with a return object from TEA.fit.MCMCregress
#' (via do.call() by addition of a "DFsub" element to the list returned by TEA.fit.MCMCregress),
#' but can be used manually.
#' @param Fit = a matrix of posterior parameter draws returned by MCMCregress
#' @param Formula = the formula used to generate Fit
#' @param Mmod = a model matrix, typically generated by model.matrix(),
#' but can be any matrix that has column names conforming to Formula.
#' @param Llev = the levels/unique values for every factor/character variable referenced in Formula.
#' Used to match levels of predicted data with that of the fitting data.  New levels in
#' predicted data will cause an error; missing levels will accounted for.
#' @return a vector containing the posterior predictive draws

TEA.regressish.est <- function(env){
	#check levels of subset versus model
	#new levels in subset means error
	#missing levels means add to levels
    attach(env$parameterModel$env)
    attach(env)
    flev <- function(var){
        Vret <- env$data[,var]
        Lret <- list(Vret)
        names(Lret) <- var
        if(is.character(env$data[,var])) Vsub <- levels(factor(env$data[,var]))
        if(is.factor(env$data[,var])) Vsub <- levels(env$data[,var])
        #if a numeric var, just return values
        else return(Lret)
        #see if there are new levels
        Vvar <- Llev[[var]]
        if(is.null(Vvar)) stop("Couldn't find variable",var,"in levels list")
        if(length(setdiff(Vsub,Vvar))>0) stop(paste("New levels found for",var,"in prediction data set"))
        #add missing levels
        if(length(setdiff(Vvar,Vsub))>0){
            Vret <- factor(env$data[,var],levels=c(levels(factor(env$data[,var])),setdiff(Vvar,Vsub)))
            Lret <- list(Vret)
            names(Lret) <- var
            return(Lret)
        }
        else return(Lret)
    }
	env$data <- as.data.frame(lapply(all.vars(Formula),flev))
	Msub <- TEAConformMatrix(model.matrix(Formula,env$data),Mmod)
	#get parameter rows
	#Vrow <- sample(1:nrow(Fit),nrow(Msub),replace=TRUE)
	#same row for all!
	Vrow <- rep(RapopModelDraw(env$parameterModel),nrow(Msub))
	#betas
	Mbeta <- Fit[Vrow,-ncol(Fit)]
    detach(env$parameterModel$env)
    detach(env)
    env$Msub <- Msub
    env$Mbeta <- Mbeta
    env$Vrow <- Vrow
}

TEA.regressish.draw <- function(env){
	Vsig <- sqrt(env$parameterModel$env$Fit[env$Vrow,ncol(env$parameterModel$env$Fit)]) #sigma
	Vmu <- diag(env$Msub %*% t(env$Mbeta))
	Vdraw <- rnorm(nrow(env$Msub),Vmu,Vsig)
	return(Vdraw)
}

regressish <- new("apop_model", name="regressish",  
                                estimate_function=TEA.regressish.est,
                                draw_function=TEA.regressish.draw)


#' Estimate a CART model on the given data
#' The function takes one argument, an environment.  The following are
#' the elements expected in the environment
#' @param data = a data frame containing the variables promised in the formula
#' @param formula = the formula used to generate fit
#' @return nothing, but update the environment with a new item:
#' fit = an object of class 'tree', giving the fit.

TEA.tree.est <- function(env){
	ffact <- function(x){
		if(is.character(x)) return(factor(x))
		else return(x)
	}
	env$Data <- as.data.frame(lapply(env$Data,ffact)) #factorize characters
	env$fit <- try(tree(env$Formula,data=env$Data,y=TRUE))
	if(inherits(env$fit,"try-error")) stop(paste("Tree fit did not work on"))
}

#' Draw synthetic values from a CART fit, usin Reiter's Bayesian bootstrap method
#' The function takes one argument, an environment.  The following are
#' the elements expected in the environment
#' @param data = a data frame containing the variables promised in the formula
#' @param formula = the formula used to generate fit
#' @param fit = an object of class 'tree' giving the fit
#' @return a vector containing the synthetic values

TEA.tree.draw <- function(env){
	#TODO
	#with newdata, first step is to assign leaves to each record
	#then proceed as below
	ffact <- function(x){
		if(is.character(x)) return(factor(x))
		else return(x)
	}
	env$Data <- as.data.frame(lapply(env$Data,ffact)) #factorize characters
	vwhere <- predict.tree(env$fit,env$Data,type="where")

	#do some prediction via Bayes Bootstrap
	#find all observations in a given leaf
	#get all leaves
	vret <- env$fit$y
	vleaf <- row.names(subset(env$fit$frame,var=="<leaf>"))
	for(kleaf in vleaf){
		#vwch <- which(rownames(env$fit$frame)[env$fit$where]==kleaf)
		vwch <- which(rownames(env$fit$frame)[vwhere]==kleaf)
		vvals <- vret[vwch]
		vsyn <- NULL
		#not sure if draw of sampling probs occurs just once per leaf...
		vunif <- c(0,runif(length(vvals)-1,0,1),1)
		vunif <- vunif[order(vunif)]
		#repeat this until you get all the values you need
		idx <- 0
		while(idx < length(vvals)){
			vuimp <- runif(length(vvals),0,1)
			vlow <- vuimp > vunif[-length(vunif)]
			vhi <- vuimp <= vunif[-1]
			vimp <- vlow & vhi
			vsyn <- c(vsyn,vvals[vimp])
			idx <- idx + sum(vimp)
		}
		vsyn <- vsyn[1:length(vvals)] #chop off excess imputes
		vret[vwch] <- vsyn
	}
	ret <- env$Data
	lhs <- all.vars(env$Formula)[1]
	ret[,lhs] <- vret
	return(ret)
}

teatree <- new("apop_model", name="teatree",  
                                estimate_function=TEA.tree.est,
                                draw_function=TEA.tree.draw)



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
	env$Llev <- sapply(Vvar,flev, env$Data)
	env$Newdata <- env$Data #set new data to data used for fit; user can modify this
}

TEA.MCMCregress.draw <- function(env){
    flev <- function(var){
        Vret <- env$Newdata[,var]
        Lret <- list(Vret)
        names(Lret) <- var
        if(is.character(env$Newdata[,var])) Vsub <- levels(factor(env$Newdata[,var]))
        if(is.factor(env$Newdata[,var])) Vsub <- levels(env$Newdata[,var])
        #if a numeric var, just return values
        else return(Lret)
        #see if there are new levels
        Vvar <- env$Llev[[var]]
        if(is.null(Vvar)) stop("Couldn't find variable",var,"in levels list")
        if(length(setdiff(Vsub,Vvar))>0) stop(paste("New levels found for",var,"in prediction data set"))
        #add missing levels
        if(length(setdiff(Vvar,Vsub))>0){
            Vret <- factor(env$Newdata[,var],levels=c(levels(factor(env$Newdata[,var])),setdiff(Vvar,Vsub)))
            Lret <- list(Vret)
            names(Lret) <- var
            return(Lret)
        }
        else return(Lret)
    }
	#trim off response from formula so model.matrix works
	env$Newdata <- as.data.frame(lapply(all.vars(env$Formula),flev))
	newform <- as.formula(paste("~",paste(all.vars(env$Formula)[-1],collapse="+")))
	Msub <- TEAConformMatrix(model.matrix(newform,env$Newdata),env$Mmod)
	#get parameter rows
	#Vrow <- sample(1:nrow(Fit),nrow(Msub),replace=TRUE)
	#same row for all!
	Vrow <- rep(sample(1:nrow(env$Fit),1),nrow(Msub))
	#betas
	Mbeta <- env$Fit[Vrow,-ncol(env$Fit)]
	Vsig <- sqrt(env$Fit[Vrow,ncol(env$Fit)]) #sigma
	Vmu <- diag(Msub %*% t(Mbeta))
	Vdraw <- rnorm(nrow(Msub),Vmu,Vsig)
	return(Vdraw)
}

mcmc.reg <- new("apop_model", name="MCMC regression",  
                                estimate_function=TEA.MCMCregress.est, 
                                draw_function=TEA.MCMCregress.draw)
