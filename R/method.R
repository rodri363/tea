#' The sequential regression multiple imputation method.
#' The function takes one argument, a list.  The following are
#' the elements expected in the list.
#' @param kdb = string giving the path to a database
#' @param kstab = string giving the table from which to select records
#' @param kutab = string giving the table to update with new records
#' @param vmatch = character vector giving the variables used for matching in the table
#' @param kuniv = string giving SQL "where clause" statements for selecting records
#' @param ksave = string giving the name for the SQLite savepoint
#' @param fupdate = a function used for updating between model fits
#' @param lform = a list of start formulae
#' @param lmodel = a list of TEA models, corresponding to lform
#' @return an environment, containing the model fits and all results

srmi <- function(srmilist){
	ee <- as.environment(srmilist)
	ee$con <- dbConnect(dbDriver("SQLite"),ee$kdb)
	#all the unique variables given in the formulas
	vvars <- unique(c(ee$vmatch,unlist(lapply(ee$lform,all.vars))))
	#ID data frame
	query <- paste("select",paste(ee$vmatch,collapse=","),"from",
		ee$kstab,"where",ee$kuniv)
	ee$DFID <- dbGetQuery(ee$con,query)
	#get records via prepped query
	#The reason for using a prepared select is that,
	#if the variables used to define the where clause are modified
	#we might get different records for each model!
	ee$prepq <- paste("select",paste(vvars,collapse=","),"from",
		ee$kstab,"where",paste(ee$vmatch,paste(":",ee$vmatch,sep=""),collapse=" and ",sep="="))
	ee$Data <- dbGetPreparedQuery(con,ee$prepq,ee$DFID)
	#setup models
	lmod <- lapply(ee$lmodel,setupRapopModel)
	#initilization stuff
	#this could also be an input function
	dbGetQuery(ee$con,paste("savepoint",ee$ksave))

	#first run
	#for each model
	for(ldx in 1:length(ee$lform)){
		print(ldx)
		fit <- estimateRapopModel(list(Formula=ee$lform[[ldx]],Data=ee$Data),lmod[[ldx]])
		ee$Data <- RapopModelDraw(fit)
		ee$kcol <- all.vars(ee$lform[[ldx]])[1]
		ee$fupdate(ee)
	}

	#change models
	#add in yj+1 ... yn for response yj
	vadd <- NULL
	for(ldx in length(ee$lform):2){
		vadd <- c(all.vars(ee$lform[[ldx]])[1],vadd)
		form <- ee$lform[[ldx-1]]
		print(vadd)
		print(form)
		ee$lform[[ldx-1]] <- update.formula(form,
			paste("~ . +",paste(vadd,collapse="+")))
	}

	#loop run
	#could also base stop on convergence criteria
	#for each model (future plans)
	kloop <- 0
	while(kloop < 10){
		for(ldx in 1:length(ee$lform)){
			print(kloop)
			print(ldx)
			fit <- estimateRapopModel(list(Formula=ee$lform[[ldx]],Data=ee$Data),
				lmod[[ldx]])
			ee$Data <- RapopModelDraw(fit)
			ee$kcol <- all.vars(ee$lform[[ldx]])[1]
			ee$fupdate(ee)
		}
		kloop <- kloop+1
	}

	#finalization stuff
	dbGetQuery(ee$con,paste("rollback to savepoint",ee$ksave))
	dbGetQuery(ee$con,paste(paste("release",ee$ksave)))
	return(ee)
}
