#need
#con a database connection
#ktab table to select from
#kupdate table to update
#korig table used to restore original values
#vsyn variables to synthesize
#vorig original variables to restore
#DFsyn the data to synthesize (must include all matching vars)
#vid id variables
#vgroup vector of grouping variables for consistency checking
#Lfit list of fits
#kmaxloop number of consistency loops to run

consistency_draw <- function(envc){
	if(envc$debug) browser()
	dbGetQuery(envc$con,"savepoint consist_save")
#	dbWriteTable(envc$con,"syntemp",envc$DFsyn[,unique(c(envc$vid,envc$vgroup))],row.names=FALSE,overwrite=TRUE)
#	dbGetQuery(envc$con,"drop index if exists syndx")
#	dbGetQuery(envc$con,
#		paste("create index syndx on syntemp(",paste(envc$vid,collapse=","),")"))
	DFid <- envc$DFsyn[,unique(c(envc$vid,envc$vgroup))]
	#write IDs to a table, make indices
	vtypes <- unlist(lapply(DFid,typeof))
	vtypes[vtypes=="character"] <- "text"
	vtypes[vtypes=="double"] <- "real"
	Mtypes <- cbind(names(DFid),vtypes)
	WriteTable(DFid,"syntemp",envc$con,Mtypes,envc$vid,overwrite=TRUE)
		
	#records left to synthesize
	kleft <- dbGetQuery(envc$con,"select count(*) as ct from syntemp")$ct
	kloop <- 0
	vfail <- NULL
	vbound <- NULL
	#browser()
	#TODO
	#get this not broken when there is 1 record left (kleft==1)
	while(kleft>1 & kloop<envc$kmaxloop){
		print(kleft)
		print(kloop)
		#get data from syntemp ids
		query <- paste("select * from",envc$ktab,"as a, syntemp as b",
			"where",
			paste(paste("a",envc$vid,sep="."),
				paste("b",envc$vid,sep="."),
				sep="=",collapse=" and "))
		envc$DFsyn <- dbGetQuery(envc$con,query)
		#synthesize the data
		#set newdata in Fit to DFsyn
		#envc$Fit$env$Newdata <- envc$DFsyn
		print("Synthesizing")
		for(fit in envc$Lfit){
			fit$env$Newdata <- envc$DFsyn
			DFtmp <- try(RapopModelDraw(fit))
			if(!inherits(DFtmp,"try-error")) envc$DFsyn <- DFtmp
			else print(paste(fit$env$Formula, "did not work"))
			print(unlist(lapply(envc$DFsyn[,envc$vsyn],function(x) sum(is.na(x)))))
		}
		print("Updating")
		UpdateTablefromDF(envc$DFsyn,envc$kupdate,envc$con,envc$vsyn,envc$vid,verbose=TRUE)
		#now that we've updated,
		#check records of all households with someone who was synthesized
		query <- paste("select * from",envc$ktab,
			"where",
			paste(sprintf("%s in (select distinct %s from syntemp)",envc$vgroup,envc$vgroup),
				collapse=" or "))
		print(query)
		DFcheck <- dbGetQuery(envc$con,query)

		print("Checking Bounds")
		#find bounds errors in age, and reupdate
			#get all edit variables
		vedvar <- dbGetQuery(envc$con,"select * from variables")$name
#		Mbnd <- sapply(vedvar, function(x) return(CheckBounds(DFcheck[,x],x)))
		Mbnd <- sapply(vedvar,function(x) return(CheckBounds(DFcheck[,x],x,envc$con)))
		Mbnd <- Mbnd==1
		Mbnd[is.na(Mbnd)] <- FALSE
		if(!is.null(nrow(Mbnd))) vbound <- as.logical(rowSums(Mbnd))
		else vbound <- Mbnd
		print("Checking Consistency")
		if(nrow(DFcheck[!vbound,])>0) vfail <- CheckDF(DFcheck[!vbound,],envc$con)
		vbad <- vbound
		vbad[!vbad] <- vfail
		#any SERIALNO with any bound or consistency failures must be run through again
		if(sum(vbad)>0){
			query <-paste("select * from syntemp where",
					paste(envc$vgroup,paste("$",envc$vgroup,sep=""),sep="=",collapse=" and "))
			print(query)
			DFtmp <- dbGetPreparedQuery(envc$con,query,
				bind.data=unique(DFcheck[vbad==1,envc$vgroup,drop=FALSE]))
			WriteTable(DFtmp,"syntemp",envc$con,Mtypes,envc$vid,overwrite=TRUE)
			kleft <- dbGetQuery(envc$con,"select count(*) as ct from syntemp")$ct
		}else kleft <- 0

		kloop <- kloop+1
	}

	print(paste("Unable to make consistent synthetic data for",kleft,"records"))

	#TODO
	#need to figure out best way to restore bad values
	#final synthetic data
	#need to reset bad records to original values
	#so insert from ORIGpdc back into pdc for anything
	#stil in syntemp
	#for now am selecting everything that wasn't still bad
	vv <- c(envc$vid,envc$vorig)
	query <- paste("select",paste("a",vv,sep=".",collapse=","),
		"from",envc$korig,"as a, syntemp as b",
		"where",
		paste(paste("a",envc$vid,sep="."),
			paste("b",envc$vid,sep="."),
			sep="=",collapse=" and "))
	DFup <- dbGetQuery(envc$con,query)
	UpdateTablefromDF(DFup,envc$kupdate,envc$con,envc$vorig,envc$vid)
	#regrab updated final data
	#rewrite syntemp to hold original IDs
	WriteTable(DFid,"syntemp",envc$con,Mtypes,envc$vid,overwrite=TRUE)
	#now regrab synthetic variables
	vv <- c(envc$vid,envc$vsyn)
	query <- paste("select",
		paste(paste("a",vv,sep="."),vv,sep=" as ",collapse=","),
		"from",envc$ktab,"as a, syntemp as b",
		"where",
		paste(paste("a",envc$vid,sep="."),
			paste("b",envc$vid,sep="."),
			sep="=",collapse=" and "))
	print(query)
	envc$DFsyn <- dbGetQuery(envc$con,query)
	#TODO
	#decide if we want to restore original data completely b4 we leave
	dbGetQuery(envc$con,"rollback to consist_save")
	dbGetQuery(envc$con,"release consist_save")
	return(envc$DFsyn)
}
	
#' Fit a "tree grove", a sequential fits of CART models
#' @param env an environment, containing at minimum the following objects
#' env$Formula a list of formulas, one for each response variable to model
#' env$Data data on which to fit the grove
#' env$Method a character vector, giving the drawing method to be used
#' for each model ("bb" or "kde")
#' returns NULL, but adds the following elements to env
#' env$lfit a list of tree fits
#' env$Newdata a copy of env$Data to be used for making draws by default
#'
#' The order used to for the fits is determined by the number of missing
#' values of each response variable; largest number goes first.  Ties
#' in the number of missing values are broken using the method of Reiter
#' (needs implementation!)
teagrove.est <- function(env){
	if(is.null(env$Formula)) stop("Groves need trees which need formulas")
	if(is.null(env$Data)) stop("Groves need data")
	if(is.null(env$Method)) env$Method <- rep("bb",length(env$Formula))
	#get lhs
	vlhs <- unlist(lapply(env$Formula,function(x) return(all.vars(x)[1])))
	#fit initial models
	env$lfit <- list()
	for(kform in env$Formula){
		klhs <- all.vars(kform)[1]
		env$lfit[[klhs]] <- as.environment(list(Formula=kform,Data=env$Data))
		teatree.est(env$lfit[[klhs]])
	}
	#determine modeling order based on # of missing items
	vmiss <- unlist(lapply(env$Data[,vlhs],function(x) sum(is.na(x))))
	vord <- rev(order(vmiss))
	#insert tie-breaker code here based on model fits
	#order everything
	vlhs <- vlhs[vord]
	env$Formula <- env$Formula[vord]
	env$Method <- env$Method[vord]
	env$lfit <- env$lfit[vord]
	env$Newdata <- env$Data
	#synthesize values, using new values for prediction down the road
	return(NULL)
}

#' Draw from a "tree grove"
#' @param env an environment, containing at minimum the following objects
#' env$Formula a list of formulas, one for each response variable to model
#' env$Method a character vector, giving the drawing method to be used
#' for each model ("bb" or "kde")
#' env$lfit a list of tree fits
#' env$Newdata data for which draws are desired
#' returns NULL but adds the following objects to env
#' env$Drawdata the drawn data
teagrove.draw <- function(env){
	if(is.null(env$Formula)) stop("Groves need formulas")
	if(is.null(env$Newdata)) stop("Need Newdata to do a draw")
	if(is.null(env$lfit)) stop("Need a list of tree fits to do a draw")
	env$lfit[[1]]$Newdata <- env$Newdata #set initial new data
	for(ldx in 1:length(env$lfit)){
		print(env$lfit[[ldx]]$Formula)
		teatree.draw(env$lfit[[ldx]])
		if(ldx<length(env$lfit)) env$lfit[[ldx+1]]$Newdata <- env$lfit[[ldx]]$Drawdata
	}
	env$Drawdata <- env$lfit[[ldx]]$Drawdata
	return(NULL)
}

tea.grove <- new("apop_model", name="grove",  
                                estimate_function=teagrove.est,
                                draw_function=teagrove.draw)
