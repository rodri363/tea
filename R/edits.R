# get_edit_vars and init_edit_list seemed unused; deleted.
# Restore via git checkout 6c8a929c .

#' read specification file into keys SQLite table
#' @param spec the configuration file to read (as character path)
#' @param nlines the maximal number of lines to read from the config
#' @return Nothing, but a db name and connection are placed in the global environment,
#'    pepenv$db_name and pepenv$con
read_spec <- function(spec,nlines=1000){
    pepenv$db_name <-.C("read_spec", spec, paste(rep("",nlines), collapse=" "))[[2]]
    pepenv$con <- dbConnect(dbDriver("SQLite"), pepenv$db_name);
    pepenv$verbosity <- 0
}

#' Interface with C-side record consistency checking
#' @param vals variable names
#' @param vars variable values
#' @param what_you_want level of detail of results
#' @param con a connection to a database
#' @param run_id unique identifier for this run
#' @param verbose verbose output?
#' 
#' @return a data frame containing allowable variable combinations
CheckConsistency <- function(vals,vars,what_you_want,con,run_id=1,na.char="NULL",verbose=FALSE){
	record_name_in 	<- as.character(vars)
	#had to add as.matrix() to do literal quoting of factor values,
	#rather than conversion to their numeric equivalents
	ud_values 		<- as.character(as.matrix(vals))
	ud_values[is.na(ud_values)] <- na.char
	if(verbose) print(paste("vars and values:"))
	if(verbose) print(rbind(record_name_in,ud_values))
	record_in_size 	<- as.integer(length(vars))
	what_you_want 	<- as.character(what_you_want)
	run_id 			<- as.integer(run_id)
	fails_edits		<- as.integer(-1)
	record_fails	<- as.integer(rep(-1, record_in_size))
	if(what_you_want=="passfail"){
		return(.C("consistency_check",
				record_name_in, ud_values, record_in_size,
				what_you_want, run_id, fails_edits, record_fails)[[6]]);
	}else{
		editmat <- .Call("RCheckConsistency",
					record_name_in, ud_values, record_in_size,
					what_you_want, run_id, fails_edits, record_fails);
		valframe <- NULL
		if(!is.null(editmat) && (nrow(editmat)>0)){
			editmat <- as.matrix(unique(editmat,MARGIN=1))
			vars <- colnames(editmat)
			query <- paste("select", paste(vars,vars,sep=".",collapse=","),
							"from", paste(vars,collapse=","), "where",
							paste(paste(vars,"rowid",sep="."),paste(":",vars,sep=""),
							sep="=",collapse=" and "));
			dbGetQuery(con,"begin")
			valframe <- dbGetPreparedQuery(con, query,
				 bind.data=as.data.frame(editmat+1,stringsAsFactors=FALSE)) ;
			dbGetQuery(con,"commit")
		}
		#return(unique(valmat,MARGIN=1)) #return unique rows
		if(verbose)	print("valframe names, rows, and values")
		if(verbose)	print(names(valframe))
		if(verbose)	print(nrow(valframe))
		return(valframe)
	}
}

#' Check all rows of a data frame for failing any edits
#' @param DF data frame you want to check
#' @param con a connection to a database that has the tables necessary for consistency checking
#' @param vars set of variables to check; if null, all variables are selected from the 'variables' table in DB
#' @return a vector giving the result (1 = fail, 0 = pass) for each row
CheckDF <- function(DF,con,vars=NULL){
	if(nrow(DF)<1) return(NULL)
	if(!all(vars %in% names(DF))) stop(paste("Missing the following consistency variables in your data frame",
		paste(setdiff(vars,names(DF)),collapse=",")))
	if(is.null(vars) & !dbExistsTable(con,"variables")) stop(paste("You need a 'variables' table",
		"in the database if vars is NULL"))
	if(is.null(vars)) vars <- dbGetQuery(con,"select * from variables")$name
	#had to add as.matrix() to do literal quoting of factor values,
	#rather than conversion to their numeric equivalents
	Mdf <- matrix(unlist(lapply(DF,as.character)),nrow=nrow(DF))
	colnames(Mdf) <- names(DF)
	if(nrow(Mdf)>1) return(apply(Mdf[,vars],1,CheckConsistency,vars,"passfail",con))
	return(CheckConsistency(Mdf[,vars],vars,"passfail",con))
}

#' Check a real/integer vector for values outside of declared consistency values
#' @param Vvar vector you want to check
#' @param kname name of the variable in the consistency system
#' @param con a connection to a database that has the tables necessary for consistency checking
#' @return a vector giving the result (1 = fail, 0 = pass) for each element of the vector
CheckBounds <- function(Vvar,kname){
	if(!is.numeric(Vvar))	stop("Bounds check only works on numeric values")
	if(is.na(kname) | is.null(kname)) stop("I need a variable name")
	ftmp <- function(x){
		if(is.na(x)) return(x)
		else return(.C("R_check_bounds",x,as.character(kname),as.integer(0))[[3]])
	}
	return(sapply(as.double(Vvar),ftmp))
}
