# get_edit_vars and init_edit_list seemed unused; deleted.
# Restore via git checkout 6c8a929c .


#' Open a database with a given name for use by tea.
#' This is normally done for you by readSpec. Use this if you don't have a spec file 
#' but want to run some queries on an already-processed database.
teaConnect <-function(dbname){
    teaenv$dbname <- dbname
    teaenv$con <- dbConnect(dbDriver("SQLite"), dbname)
    .C("db_open", dbname)
}

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
    
    teaenv$db_name <- .C("read_spec", spec, paste(rep("",nlines), collapse=" "))[[2]]

    if(nchar(teaenv$db_name)>0) {
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
	return(as.data.frame(.Call("RCheckData",df)))
}

blankOne <- function(r, tabname, idcolname, id){
    m <- max(r) # greater than zero, because of the if statement in the caller below.
    blankme <- sample(names(r)[r[]==m], 1)
    print(paste("update", tabname, "set", blankme, "=NULL where", idcolname, "=", id, sep=" "))
    dbGetQuery(teaenv$con, paste("update", tabname, "set", blankme, "=NULL where", idcolname, "=", id, sep=" "))
}

EditTable <- function(tabname, where=NULL){
    t <- teaTable(tabname, where=where)
    idcolname <- teaGetKey("id")
    idcol <- teaTable(tabname, cols=idcolname, where=where)
    fail <- TRUE
    while (fail) {
        fail <- FALSE
        glitches <- as.data.frame(.Call("RCheckData",t))

        for (i in 1:nrow(glitches)){
            r <- glitches[i,]
            if (sum(r)>0){
                fail <- TRUE
                blankOne(r, tabname, idcolname, idcol[[i]])
            }        
        }

        if (fail){
            .C("impute", as.character(tabname))
        }
        #These are rowids where we couldn't draw a consistent record
        hardFails <- teaTable("tea_fails")
    }
}
