# get_edit_vars and init_edit_list seemed unused; deleted.
# Restore via git checkout 6c8a929c .

#' read specification file into keys SQLite table
#' @param spec the configuration file to read (as character path)
#' @param nlines the maximal number of lines to read from the config
#' @return Nothing, but a db name and connection are placed in the global environment,
#'    teaenv$db_name and teaenv$con
readSpec <- function(spec,nlines=1000){
    options(warn=1) #print warnings as they occur.
    skip <- 0

    # Check whether, during execution of read_spec, the database has been successfully
    # written to the keys table. If not, then don't perform dbConnect below (and just go
    # back to R after displaying warning message).
    
    browser()

    teaenv$db_name <- tryCatch({
    .C("read_spec", spec, paste(rep("",nlines), collapse=" "))[[2]]
    }, warning=function(war) {

    if(war$message == "TEA was unable to read your spec file. This is most likely due to the fact that you didn't specify a database at the header of the file.") {
       skip <<- skip + 1;
    }

    })

    print(paste(skip))

    # If skip==1 then database was not written to spec file to executing 
    # function below will cause error.
    if(!skip) {
        teaenv$con <- dbConnect(dbDriver("SQLite"), teaenv$db_name);
    }
    teaenv$verbosity <- 0
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
		if(is.null(editmat)) return(valframe)
		editmat <- as.data.frame(editmat)
		editmat$Vector <- NULL
		if((nrow(editmat)>0)){
			#changed editmat+1 to editmat with new C code
			editmat <- as.matrix(unique(editmat,MARGIN=1)) + 1
			vars <- colnames(editmat)
			query <- paste("select", paste(vars,vars,sep=".",collapse=","),
							"from", paste(vars,collapse=","), "where",
							paste(paste(vars,"rowid",sep="."),paste(":",vars,sep=""),
							sep="=",collapse=" and "));
			dbGetQuery(con,"begin")
			valframe <- dbGetPreparedQuery(con, query,
				 bind.data=as.data.frame(editmat,stringsAsFactors=FALSE)) ;
			dbGetQuery(con,"commit")
		}
		#return(unique(valmat,MARGIN=1)) #return unique rows
		if(verbose)	print("valframe names, rows, and values")
		if(verbose)	print(names(valframe))
		if(verbose)	print(nrow(valframe))
		return(valframe)
	}
}

#' Check a real/integer vector for values outside of declared consistency values
#' @param Vvar vector you want to check
#' @param kname name of the variable in the consistency system
#' @param con a connection to a database that has the tables necessary for consistency checking
#' @return a vector giving the result (1 = fail, 0 = pass) for each element of the vector
CheckBounds <- function(Vvar,kname){
	if(is.na(kname) | is.null(kname)) stop("I need a variable name")
	ftmp <- function(x){
		if(is.na(x)) return(0)
		else return(.C("R_check_bounds",x,as.character(kname),as.integer(0))[[3]])
	}
	return(sapply(as.double(Vvar),ftmp))
}

CheckBounds <- function(Vvar,kname,con){
	if(is.na(kname) | is.null(kname)) stop("I need a variable name")
	Vset <- dbGetQuery(con,paste("select * from",kname))[,kname]
	return(as.integer(!(Vvar %in% Vset)))
}

CheckDF <- function(df){
	return(.Call("RCheckData",df))
}
