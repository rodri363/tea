#' Prepare data for entry into seqRegAICfit.
#'
#' @param model.spec a list of modeling options, identical to what is provided for seqRegAICfit():
#' \enumerate{
#' \item [[*]][[1]] = an R formula (either as a formula object or a character),
#' giving the initial model to fit on the data.
#' \item [[*]][[2]] = a length 1 character containing the name of a supported regression function
#' \item [[*]][[3]] = a character vector containing the optional right-hand-side variables
#' }
#' Each [[*]] element represents a new LHS variable for modeling.
#' @param data frame for preparation
#' @return prepared data frame.  Currently, the following modification are made:
#' Variables to be modeled with an ordinal model are recoded as ordered factors
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}
#' @TODO need to fix errors in factor codings (e.g. 0-3 as factor is numerically 1-4)

seqRegAICprep <- function(model.spec, data){
	#need to drop all missing factor levels
	#tmpF <- function(x){
	#	if(is.factor(x)){
	#		x <- factor(x)
	#	}
	#	return(x)
	#}
	#data <- as.data.frame(lapply(data[,names(data)],tmpF))
	for(ldx in 1:length(model.spec)){
		lhs <- all.vars(as.formula(model.spec[[ldx]][[1]]))[1]	#extract LHS variable
		modelfun <- model.spec[[ldx]][[2]]			#extract model function

		#make ordered factors for ordinal models
		if(modelfun=="polr"){
			#have to change R encoding of variable to ordered factor
			#for polr to work with it as the lhs variable
			data[,lhs] <- ordered(data[,lhs])
		}
	}
	return(data)
}

#' See doSequentialRegression for details
#'
#' @param model.spec a list of modeling options, specified as:
#' \enumerate{
#' \item [[*]][[1]] = an R formula (either as a formula object or a character),
#' giving the initial model to fit on the data.
#' \item [[*]][[2]] = a length 1 character containing the name of a supported regression function
#' \item [[*]][[3]] = a character vector containing the optional right-hand-side variables
#' }
#' Each [[*]] element represents a new LHS variable for modeling. The order of model
#' fitting is decided by the ordering of the list.
#' @param data a data frame containing, at a minimum, all variables found in model.spec
#' @seealso stepAICtry
#' @return a list containing:
#' \item{[[*]][[1]]}{a length 1 character containing the name of a supported regression function}
#' \item{[[*]][[2]]}{a length 1 character containing the final fitted formula}
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}

seqRegAICfit <- function(model.spec, data){
	nobs <- nrow(data)		#number of obs in data
	rlist <- vector("list",length(model.spec))
	for(ldx in 1:length(model.spec)){
		model.formula <- as.formula(model.spec[[ldx]][[1]])				#extract structural model
		lhs <- all.vars(model.formula)[1]								#extract LHS variable
		modelfun <- model.spec[[ldx]][[2]]								#extract model function
		preds.req <- all.vars(as.formula(model.spec[[ldx]][[1]]))[-1]	#extract required RHS variables
		preds.opt <- model.spec[[ldx]][[3]]								#extract optional RHS variables

		#if there are fewer than 3 levels for the variable
		#and using an ordinal logistic regression model,
		#switch to a multinomial (bernoulli) model
		if(length(table(data[,lhs]))<3 & modelfun=="polr"){
			modelfun <- "multinom"
			print(paste("Fewer than 3 response categories, changing from polr to multinom in domain"))
		}

		#Number of steps to consider is based on number of data points
		#in the subdomain
		nsteps <- min(6,nobs/2)
		##set model options (iterations, convergence, etc.)
		modopts <- ""
		if(modelfun=="polr"){
			modopts <- paste(",control=list(reltol=1e-4,maxit=500,trace=0)")
			nsteps <- min(6,nobs/(2*length(levels(data[,lhs]))))
		}
		if(modelfun=="multinom"){
			modopts <- c(",reltol=1e-3,maxit=500,trace=FALSE")
			nsteps <- min(6,nobs/(2*length(levels(data[,lhs]))))
		}

		#remove response variable from predictor list
		preds.req <- preds.req[which(preds.req != lhs)]
		preds.opt <- preds.opt[which(preds.opt != lhs)]

		preds.all <- c(preds.req,setdiff(preds.opt,preds.req)) #all predictors
		#determine which predictors to include in interactions
		#current criterion is < 6 levels for two-way
		#and < 4 levels for three-way interactions
		twoway.preds <- NULL
		threeway.preds <- NULL
		drop.preds <- NULL
		for(var in preds.all){
			if(is.factor(data[,var])){
				if(length(levels(factor(data[,var]))) < 6){
					twoway.preds <- c(twoway.preds,var)
				}
				if(length(levels(factor(data[,var]))) < 4){
					threeway.preds <- c(threeway.preds,var)
				}
				#drop predictors with too many levels for multinom
				#they really bog down multinomial models
				if(length(levels(factor(data[,var]))) > 10 &
					modelfun=="multinom"){
					drop.preds <- c(drop.preds,var)
				}
			}
		}
		twoway.preds <- setdiff(twoway.preds,threeway.preds)
		single.preds <- setdiff(preds.all,c(twoway.preds,threeway.preds,drop.preds))
		#add smoothing language for numeric variables for GAM
		if(modelfun=="gam"){
			for(vardx in 1:length(single.preds)){
				varname <- single.preds[vardx]
				if(is.numeric(data[,varname])){
					single.preds[vardx] <- paste("s(",varname,")",sep="")
				}
			}
		}
		if(length(twoway.preds)==1){
			single.preds <- c(single.preds,twoway.preds)
			twoway.preds <- NULL
		}
		if(length(threeway.preds)==1){
			single.preds <- c(single.preds,threeway.preds)
			threeway.preds <- NULL
		}
		#maximal model (use all possible predictors and interactions)
		stepUp <- as.formula(paste(lhs," ~ ",
			paste(single.preds,collapse="+"),
			ifelse(length(twoway.preds)>1,
				paste("+","(",
					paste(twoway.preds,collapse="+"),
					")^2",sep=""),""),
			ifelse(length(threeway.preds)>1,
				paste("+","(",
					paste(threeway.preds,collapse="+"),
					")^3",sep=""),"")
			))
		#print(stepUp)

	
		#fit initial model
		#with new config should be easy to add
		#separate initial models for each variable
		
		#if no required predictors, fit the intercept
		if(is.null(preds.req) | length(preds.req)==0) preds.req <- "1"
		#define initial model based on required predictors
		#initmod <- paste(modelfun,
		#	"(",lhs,"~",paste(preds.req,collapse="+"),",",
		#	"data = data",modopts,")")
		initmod <- paste(modelfun,"(",deparse(model.formula),", data = data",modopts,")")
		#print("Trying initial model fit:")
		#print(initmod)
		initfit <- try(eval(parse(
			text=initmod)),silent=TRUE)

		#run stepAIC, starting with initial intercept-only model,
		#with the given number of steps.
		#Note that I really call stepAICtry, which is my hacked
		#version of stepAIC that actually catches errors
		#in the model fits.  This lets us try all the models
		#without getting stopped prematurely for computational reasons
		#(can't find starting values, random other errors).
		#For certain functions (polr()), "warnings" can still produce
		#problems in prediction, so we convert warnings to errors
		#for this step
		options(show.error.messages=FALSE)
		options(warn=2)

		if(!inherits(initfit,"try-error")){
			fit <- stepAICtry(initfit,stepUp,
				direction="forward",steps=nsteps)
			if(inherits(fit,"try-error")){
				fit <- initfit
			}
			rlist[[ldx]] <- list(deparse(formula(fit)),modelfun)
		}else{
			stop("Initial fit failed")
		}
		options(warn=0)
	}
	return(rlist)
}

#' Given a set of model fits from seqRegAICfit, produce synthetic data
#' using sequential regression.
#'
#' @param model.fit model fits as returned by seqRegAICfit, which is of form:
#' \enumerate{
#' \item [[*]][[1]] = an R formula (either as a formula object or a character),
#' giving the initial model to fit on the data.
#' \item [[*]][[2]] = a length 1 character containing the name of a supported regression function
#' }
#' @param syn.method a list (with the same length as model.fit) giving the synthesis methodology to
#' use for each model. If NULL, default methods are used, currently:
#' \itemize{
#' \item gam = predictive mean matching
#' \item multinom = multinomial draw from fitted probabilities
#' \item polr = multinomial draw from fitted probabilities
#' }
#' @param data the data set for synthesis
#' @param flags a list having the following elements:
#' \enumerate{
#' \item [[*]][[1]] = data set containing flags (with an ID variable matching the id parameter)
#' \item [[*]][[2]] = a list of the same length as model.fit. Each list item is a character
#' vector, giving the columns of the flagging data set to use for flags for the corresponding
#' left-hand-side variable from model.fit. That is, flags[[*]][[2]][i] gives the flags
#' for model.fit[[i]]
#' @param id unique identifier variable in the data set
#' @param consistency if not NULL, a list (in the same variable order as model.fit) giving sentinel
#' values for the consistency system
#' @param con database connection to use for consistency checking. Default = pepenv\$con.
#' @param bayes re-fit model using MCMC methods?
#' @param ncore number of processor cores to use in mclapply. Default is 1
#' @param verbose verbose output?
#' @return a new data frame with synthetic values
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}

seqRegAICsyn <- function(model.fit, syn.method, data, flags=vector("list",2), id = "ID", consistency = NULL, con = pepenv$con,
	bayes = FALSE, ncore=1, verbose=FALSE){

	all.lhs <- character(0)
	if(!all(id %in% names(data))) stop(paste("ID variable(s)",id,"not found in data"))
	options(show.error.messages=TRUE)
	options(warn=0)
	if(!is.null(consistency)){
		sentinels <- consistency
		consistency <- TRUE
	}else{
		consistency <- FALSE
		sentinels <- vector("list",length(model.fit))
	}
	if(consistency){
		if(is.null(con)) stop("Consistency checking requires a database connection with a loaded config file")
		#need all variables being modified to feed into consistency system
		edit.vars <- dbGetQuery(con,"select name from variables")[,"name"]
		print(paste("Using the following variables for consistency:",paste(edit.vars,collapse=",")))
	}

	for(ldx in 1:length(model.fit)){
		edit.sent <- sentinels[[ldx]]
		edit.wch <- NULL
		editable <- FALSE
		t1 <- proc.time()
		mod <- as.formula(model.fit[[ldx]][[1]])
		lhs <- all.vars(mod)[1]
		all.lhs <- c(all.lhs,lhs)
		print(paste("Synthesizing",lhs))
		modelfun <- model.fit[[ldx]][2]

		#get flagged rows for variable
		if(nrow(flags[[1]])==0){
			print("Nothing flagged, returning original data")
			return(data)
		}
		if(!is.null(flags[[1]]) & length(flags[[2]]) == length(model.fit)){
			#find rows that have a non-zero flag value on any of the flag columns
			#for the lhs in question
			flgrws <- unique(which(flags[[1]][ flags[[2]][ldx] ]>0,arr.ind=TRUE)[,1])
			print(paste("There are",length(flgrws),"flagged records for",lhs))
		}else{
			print("Flagging everyone")
			flgrws <- 1:nrow(data)
		}

		#see if lhs variable has consistency rules attached to it
		if(consistency) editable <- lhs %in% edit.vars

		#if there are fewer than 3 levels for the variable
		#and using an ordinal logistic regression model,
		#switch to a multinomial (bernoulli) model
		if(length(table(data[,lhs]))<3 & modelfun=="polr"){
			modelfun <- "multinom"
			print(paste("Changing from polr to multinom in domain",domain))
		}
		##set model options (iterations, convergence, etc.)
		if(!bayes){
			if(modelfun=="polr")		fit <- PEP.polr(mod,data)
			if(modelfun=="multinom")	fit <- PEP.multinom(mod,data)
		}else{
			if(modelfun=="polr")		fit <- PEP.MCMCoprobit(mod,data)
			if(modelfun=="multinom")	fit <- PEP.MCMCmnl(mod,data)
		}
		if(modelfun=="gam")	fit <- PEP.gam(mod,data)
		#if model fit worked, synthesize, otherwise move on
		if(!inherits(fit,"try-error")){
			#####################
			#Synthesis
			#####################
			#use models and edits to generate synthetic data

			#what to do if we're synthesizing from a logistic model
			#currently supports one option: draw from multinomial with fitted probs. per record
			if(modelfun %in% c("polr","multinom")){
				#fitted data list
				#contains the ID variable, the synthesized variable
				#the fitted values (probabilities) from the model,
				#and the levels of the synthesized variable
				fitdat <- list(ID=data[id],VAR=data[,lhs],
					FIT=fit$probs,
					LEV=fit$lev)
				#actual synthesis
				#for each flagged record:

				#can multicore this
				#but may not help due to overhead
				f.tmp <- function(rec){
					rec.alts <- unique(data[,lhs])
					recID <- fitdat$ID[rec,]
					#datRow <- which(data$ID==recID)
					#datRow <- which(apply(data[id]==unlist(recID),1,all))
					datRow <- which(apply(mapply("==",lapply(data[id],as.vector),as.vector(recID)),1,all))
					if(verbose) print(datRow)
					#fitted probabilities for the record
					recprob = fitdat$FIT[rec,]
					#handle consistency checking
					if(editable){
						edit.vals <- as.character(as.matrix(data[datRow,edit.vars]))
						edit.wch <- which(edit.vars %in% lhs)
						edit.vals[edit.wch] <- edit.sent
						rec.mat <- CheckConsistency(edit.vals,vars=edit.vars,
							what_you_want="find_alternatives",con=con,
							verbose=verbose);
						if(!is.null(rec.mat)){
							if(lhs %in% names(rec.mat)){
								rec.alts <- as.character(as.matrix(unique(rec.mat[,lhs])))
								#remove inconsistent levels
								recprob[!(fitdat$LEV %in% rec.alts)] <- 0
							}
						}
					}
					#re-weight remaining levels
					recprob <- recprob/sum(recprob)
					#draw a synthetic value from the final probabilities
					draw <- try(sample(fitdat$LEV,1,prob=recprob))
					if(inherits(draw,"try-error")){
						print(paste("Record",datRow,"had a missing prob."))
						print(recprob)
						print(fitdat$LEV)
						print(fitdat$FIT[rec,])
						draw <- fitdat$VAR[rec]
					}
					return(list(datRow=datRow,draw=draw))
				}
				flgrws <- as.list(flgrws)
				draws <- unlist(mclapply(flgrws,f.tmp,mc.cores=ncore))
				dataRows <- grep("datRow",names(draws))
				data[draws[dataRows],lhs] <- draws[-dataRows]
			}

			#what to do if we're synthesizing from a generalized additive model
			if(modelfun=="gam"){
				#data frame containing ID variable, synthesized variable,
				#and fitted values from gam model
				fitdat <- list(ID=data[id],VAR=data[,lhs],FIT=fit)

				f.tmp <- function(rec){
					rec.alts <- unique(data[,lhs])
					recID <- fitdat$ID[rec,]
					#datRow <- which(data$ID==recID)
					#datRow <- which(apply(data[id]==unlist(recID),1,all))
					datRow <- which(apply(mapply("==",lapply(data[id],as.vector),as.vector(recID)),1,all))
					if(verbose) print(datRow)
					if(editable){
						edit.vals <- as.character(as.matrix(data[datRow,edit.vars]))
						edit.wch <- which(edit.vars %in% lhs)
						edit.vals[edit.wch] <- edit.sent
						rec.mat <- CheckConsistency(edit.vals,vars=edit.vars,
							what_you_want="find_alternatives",con=con,
							verbose=verbose);
						#if there are alternatives
						if(!is.null(rec.mat)){
							#if the lhs is one of the edited variables
							if(lhs %in% names(rec.mat)){
								rec.alts <- as.character(as.matrix(unique(rec.mat[,lhs])))
							}
						}
					}
					#compute absolute distance of fitted values for all records
					#to the fitted value for the flagged record
					dists <- abs(fitdat$FIT[rec]-fitdat$FIT)
					#if there's more than one edit compliant value
					if(length(rec.alts)>0){
						#set non-compliant values to have NA distance
						is.na(dists[!(fitdat$VAR %in% rec.alts)]) <- TRUE
						is.na(dists[rec]) <- TRUE

						#if there are no donors, set record to have minimal distance
						if(all(is.na(dists))) dists[rec] <- 1

						#get records with minimal distance as donors
						dons <- which(dists==min(dists,na.rm=TRUE))

						#if there's only one donor record, repeat it.
						#this is necessary to handle the functionality of sample()
						if(length(dons)==1) dons <- rep(dons,2)
						
						#sample from donors to get synthetic value
						draw <- sample(fitdat$VAR[dons],1)
						#fitdat$VAR[rec] <- draw

					#if there are no edit compliant values
					#keep records as it is
					}else{
						draw <- data[datRow,lhs]
						print(paste("No donors for record",rec))
						#fitdat$VAR[rec] <- fitdat$VAR[rec]
					}
					if(verbose){
						print(lhs)
						print(data[datRow,])
						print("Original Value")
						print(data[datRow,lhs])
						#print("Matrix")
						#if(!is.null(rec.mat)) print(rec.mat)
						print("Alts")
						print(rec.alts)
						print("Drawn Value")
						print(draw)
					}
					
					#data[datRow,lhs] <- fitdat$VAR[rec]
					#if(verbose){
					#	print("Final Value")
					#	print(data[datRow,lhs])
					#	print("------")
					#}
					return(list(datRow=datRow,draw=draw))
				}
				flgrws <- as.list(flgrws)
				draws <- unlist(mclapply(flgrws,f.tmp,mc.cores=ncore))
				dataRows <- grep("datRow",names(draws))
				data[draws[dataRows],lhs] <- draws[-dataRows]
			}
		}else{
		print(paste("Couldn't find model for",lhs))
	}
	t2 <- proc.time()
	#print(paste("Time for",lhs))
	#print(t2-t1)
	}
	return(data.frame(data[c(id,all.lhs)],stringsAsFactors=FALSE))
}

