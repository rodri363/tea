srmi.fupdate.default <- function(Data,kutab,kstab,con,vupdate,vmatch,vgrab=names(Data)){
	#get IDs from Data
	#will use these to re-extract and return new data
	DFid <- Data[,vmatch]
	#update table to re-establish correct recodes
	#this is especially important if you're doing consistency checking
	UpdateTablefromDF(Data,kutab,con,vupdate,vmatch,verbose=TRUE)
	#Re-get data via IDs
	query <- paste("select",paste(unique(c(vgrab,vmatch)),collapse=","),"from",kstab,
		"where",paste(vmatch,paste(":",vmatch,sep=""),sep="=",collapse=" and "))
	print(query)
	DFret <- dbGetPreparedQuery(con,query,bind.data=DFid)
	return(DFret)
}

#TODO
#CORRECT ARGUMENTS
#Get sub-data paradigm working
#two "where" clauses:
#1	How to select data for modeling
#2	How to select data for updating
#e.g. model all the sent in data, but only update missing/flagged data

#' The sequential regression multiple imputation method.
#' The function takes one argument, a list.  The following are
#' the elements expected in the list.
#' @param kdb = string giving the path to a database
#' @param Data = data for modeling
#' @param kstab = string giving the table from which to select records
#' @param kutab = string giving the table to update with new records
#' @param vmatch = character vector giving the variables used for matching in the table
#' @param ksave = string giving the name for the SQLite savepoint
#' @param fupdate = a function used for updating between model fits
#' @param lform = a list of start formulae
#' @param lmodel = a list of TEA models, corresponding to lform
#' @return an environment, containing the model fits and all results

srmi.est <- function(esrmi){
	#esrmi <- as.environment(lsrmi)
	DFo <- esrmi$Data #keep original copy
		if(is.null(esrmi$kloop)) esrmi$kloop <- 10
		if(is.null(esrmi$kdb)) warning("No database interface; recode updates will not occur")
		if(!is.null(esrmi$kdb)) con <- dbConnect(dbDriver("SQLite"),esrmi$kdb)
		if(is.null(esrmi$lform)) stop("I need a list of formulas (lform)")
	vvars <- unique(c(esrmi$vmatch,unlist(lapply(esrmi$lform,all.vars))))
	#setup models
		if(is.null(esrmi$lmodel)) stop("I need a list of Rapop models for each formula")
		if(length(esrmi$lmodel) != length(esrmi$lform)) stop("# of models must match # of formulas")
	lmod <- lapply(esrmi$lmodel,setupRapopModel)

	#if using database, start a savepoint
	if(!is.null(esrmi$kdb) & is.null(esrmi$ksave)) esrmi$ksave <- "srmi_save"
	if(!is.null(esrmi$kdb))	dbGetQuery(con,paste("savepoint",esrmi$ksave))
	#list to keep track of missing values
	lna <- vector("list",length(esrmi$lform))
	#first run
	#for each model
	for(ldx in 1:length(esrmi$lform)){
		print(paste("Model",ldx))
		print(esrmi$lform[[ldx]])
		#get lhs variable
		lhs <- all.vars(esrmi$lform[[ldx]])[1]
		#get data with non-missing lhs values
		vna <- is.na(esrmi$Data[,lhs])
		lna[[ldx]] <- vna
		DFmod <- esrmi$Data[!vna,]
		fit <- estimateRapopModel(list(Formula=esrmi$lform[[ldx]],Data=DFmod),lmod[[ldx]])
		#set newdata for model to rows with missing lhs
		fit$env$Newdata <- esrmi$Data[vna,]
		#draw completion values and sub into Data for missing records
		#if no missing records, nothing to update
		if(sum(vna)>0) esrmi$Data[vna,lhs] <- RapopModelDraw(fit)[,lhs]
		esrmi$kcol <- lhs
		#if using a database, regrab data to get recodes corrected
		if(!is.null(esrmi$kdb))
		#need all records new, due to group level recodes
		esrmi$Data <- srmi.fupdate.default(esrmi$Data,
				esrmi$kutab,esrmi$kstab,con,
				lhs,esrmi$vmatch,vvars)
	}

	loform <- esrmi$lform
	#change models
	#add in yj+1 ... yn for response yj
	vadd <- NULL
	for(ldx in length(esrmi$lform):2){
		vadd <- c(all.vars(esrmi$lform[[ldx]])[1],vadd)
		form <- esrmi$lform[[ldx-1]]
		esrmi$lform[[ldx-1]] <- update.formula(form,
			paste("~ . +",paste(vadd,collapse="+")))
	}

	#loop run
	#in future will base stop on convergence criteria
	loopdx <- 0
	while(loopdx < esrmi$kloop){
		for(ldx in 1:length(esrmi$lform)){
			print(loopdx)
			print(ldx)
			print(esrmi$lform[[ldx]])
			lhs <- all.vars(esrmi$lform[[ldx]])[1]
			vna <- lna[[ldx]]
			#fit on all data, now complete
			fit <- estimateRapopModel(list(Formula=esrmi$lform[[ldx]],Data=esrmi$Data),
				lmod[[ldx]])
			fit$env$Newdata <- esrmi$Data[vna,]
			#draw completion values and sub into Data for missing records
			esrmi$Data[vna,lhs] <- RapopModelDraw(fit)[,lhs]
			esrmi$kcol <- lhs
			if(!is.null(esrmi$kdb))
			esrmi$Data[vna,] <- srmi.fupdate.default(esrmi$Data[vna,lhs],
					esrmi$kutab,esrmi$kstab,con,
					lhs,esrmi$vmatch,vvars)
		}
		loopdx <- loopdx+1
	}
	#once looping is done, refit *original* models on completed data
	#this is RRs thing, not in the original SRMI method
	#reset formulas
	esrmi$lform <- loform
	#list of fits to save
	esrmi$lfit <- vector("list",length(esrmi$lform))
	for(ldx in 1:length(esrmi$lform)){
		print(paste("Model",ldx))
		print(esrmi$lform[[ldx]])
		#get lhs variable
		lhs <- all.vars(esrmi$lform[[ldx]])[1]
		#get data with non-missing lhs values
		fit <- estimateRapopModel(list(Formula=esrmi$lform[[ldx]],Data=esrmi$Data),lmod[[ldx]])
		esrmi$lfit[[ldx]] <- fit
	}

	esrmi$Newdata <- DFo
	#if using database, end savepoint and restore data
	if(!is.null(esrmi$kdb))	dbGetQuery(con,paste("rollback to",esrmi$ksave))
	if(!is.null(esrmi$kdb))	dbGetQuery(con,paste("release",esrmi$ksave))
	return(esrmi)
}

#expect new esrmi$Newdata element
#go through each fit, make data, sub into Newdata, and return Newdata
srmi.draw <- function(esrmi){
	if(is.null(esrmi$Newdata)) stop("Need Newdata data frame for draws")
	for(ldx in 1:length(esrmi$lfit)){
		print(esrmi$lform[[ldx]])
		lhs <- all.vars(esrmi$lfit[[ldx]]$env$Formula)[1]
		#give model Newdata for draw
		esrmi$lfit[[ldx]]$env$Newdata <- esrmi$Newdata
		#draw completion values and sub into Data for missing records
		esrmi$Newdata[,lhs] <- RapopModelDraw(esrmi$lfit[[ldx]])[,lhs]
	}
	return(esrmi$Newdata)
}

teasrmi <- new("apop_model", name="srmi",  
                                estimate_function=srmi.est,
                                draw_function=srmi.draw)
