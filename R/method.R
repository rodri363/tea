srmi.fupdate.default <- function(Data,kutab,kstab,con,vupdate,vmatch,vgrab=names(Data)){
	#browser()
	#update table to re-establish correct recodes
	#this is especially important if you're doing consistency checking
	UpdateTablefromDF(Data,kutab,con,vupdate,vmatch,verbose=TRUE)
	#Re-get data via IDs
	query <- paste("select",
		paste(paste("a",unique(c(vgrab,vmatch)),sep="."),paste(unique(c(vgrab,vmatch))),sep=" as ",collapse=","),
		"from",kstab,
		"as a,srmi_temp as b where",
		paste(paste("a",vmatch,sep="."),paste("b",vmatch,sep="."),sep="=",collapse=" and "))
	DFret <- dbGetQuery(con,query)
#	query <- paste("select",paste(unique(c(vgrab,vmatch)),collapse=","),"from",kstab,
#		"where",paste(vmatch,paste(":",vmatch,sep=""),sep="=",collapse=" and "))
#	print(query)
#	DFret <- dbGetPreparedQuery(con,query,bind.data=DFid)
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
#DON'T DO THIS, SEND IN A CONNECTION!! 
#' @param kdb = string giving the path to a database
#' @param con = a database connection
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
	#browser()
	DFo <- esrmi$Data #keep original copy
	if(is.null(esrmi$kloop)) esrmi$kloop <- 10
		if(is.null(esrmi$con)) warning("No database interface; recode updates will not occur")
		if(is.null(esrmi$lform)) stop("I need a list of formulas (lform)")
	vvars <- unique(c(esrmi$vmatch,unlist(lapply(esrmi$lform,all.vars))))
	#setup models
		if(is.null(esrmi$lmodel)) stop("I need a list of Rapop models for each formula")
		if(length(esrmi$lmodel) != length(esrmi$lform)) stop("# of models must match # of formulas")
	lmod <- lapply(esrmi$lmodel,setupRapopModel)
	
	#if using a database, write table of IDs from Data
	#this will be used to redraw data to update recodes as
	#synthesis goes along
	#get IDs from Data
	#will use these to re-extract and return new data
	if(!is.null(esrmi$con)){
		DFid <- esrmi$Data[,esrmi$vmatch]
		vtypes <- unlist(lapply(DFid,typeof))
		vtypes[vtypes=="character"] <- "text"
		vtypes[vtypes=="double"] <- "real"
		Mtypes <- cbind(names(DFid),vtypes)
		#TODO dbWriteTable starts a transaction!!!!
		#put this automatic stuff in WriteTable
		#write to a temp table
		#dbWriteTable(esrmi$con,"srmi_temp",DFid,row.names=FALSE,overwrite=TRUE)
		WriteTable(DFid,"srmi_temp",esrmi$con,Mtypes,names(DFid),overwrite=TRUE)
	}

	#if using database, start a savepoint
	if(!is.null(esrmi$con) & is.null(esrmi$ksave)) esrmi$ksave <- "srmi_save"
	if(!is.null(esrmi$con))	dbGetQuery(esrmi$con,paste("savepoint",esrmi$ksave))
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
		if(!is.null(esrmi$con))
		#need all records new, due to group level recodes
		esrmi$Data <- srmi.fupdate.default(esrmi$Data,
				esrmi$kutab,esrmi$kstab,esrmi$con,
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

	#browser()
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
			if(!is.null(esrmi$con))
			esrmi$Data[vna,] <- srmi.fupdate.default(esrmi$Data,
					esrmi$kutab,esrmi$kstab,esrmi$con,
					lhs,esrmi$vmatch,vvars)[vna,]
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
	if(!is.null(esrmi$con))	dbGetQuery(esrmi$con,paste("rollback to",esrmi$ksave))
	if(!is.null(esrmi$con))	dbGetQuery(esrmi$con,paste("release",esrmi$ksave))
	if(!is.null(esrmi$con)) dbGetQuery(esrmi$con,"drop table srmi_temp")
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
	while(kleft>0 & kloop<envc$kmaxloop){
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
		vbound <- as.logical(rowSums(Mbnd))
		print("Checking Consistency")
		vfail <- CheckDF(DFcheck[!vbound,],envc$con)
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
	
