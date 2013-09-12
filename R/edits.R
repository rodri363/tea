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
    
    #browser()

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
GetAlternatives <- function(dfrow){
	if(nrow(dfrow)>1) stop("can only send in one row at time for now to get alts")
	dfidx <- as.data.frame(.Call("r_row_alts",dfrow)[-1])
	dfidx <- as.data.frame(lapply(dfidx,"+",1)) #add one to get sqlite indices
	vvar <- names(dfidx)
	query <- paste("select * from",paste(vvar,collapse=","),
		"where",paste(paste(vvar,"rowid",sep="."),paste("$",vvar,sep=""),
					sep="=",collapse=" and "))
	return(dbGetPreparedQuery(teaenv$con,query,dfidx))
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
