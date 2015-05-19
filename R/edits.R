# get_edit_vars and init_edit_list seemed unused; deleted.
# Restore via git checkout 6c8a929c .


#' Open a database with a given name for use by tea.
#' This is normally done for you by readSpec. Use this if you don't have a spec file 
#' but want to run some queries on an already-processed database.
teaConnect <-function(dbname){
    teaenv$dbname <- dbname
    if (!is.null(try(teaenv$con))) dbDisconnect(teaenv$con)
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
    
    dbname <- .C("read_spec", spec, paste(rep("",nlines), collapse=" "))[[2]]

    if(nchar(dbname)>0) teaConnect(dbname)
    else Warning("Couldn't read the database name from the spec file.")
    teaenv$verbosity <- 0
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

#' Check a data frame for failed edits.
#' If an edit has an associated action (e.g., the "sex=F" part of sex=M and status="pregnant" => sex=F)
#' then make these changes to the data iff do_preedits=1. If such a change is made,
#' all edits are re-checked from the start, but all preedits up to and including the
#' last one to be used will not be implemented (so on the future iterations, pregnant men
#' will still be marked as an error, but will not be changed by this rule).

#' @param df A data frame to be checked, probably generated via tea_table
#' @param do_preedits 1=Apply preedits to change the data if an error is found;
#'        0=only count errors; make no changes. Default=1.
#' @return A data frame of the same shape as the input frame. Each cell has the
#'         failure count for the corresponding input record/field. The count is
#'         after the last preedit has been run, so if you want the cont of failures
#'         in the raw data, use do_preedits=0.

CheckDF <- function(df, do_preedits=1){
	return(as.data.frame(.Call("RCheckData",df, as.integer(do_preedits))))
}

blankOne <- function(r, tabname, idcolname, id){
    m <- max(r) # greater than zero, because of the if statement in the caller below.
    blankme <- sample(names(r)[r[]==m], 1)
    print(paste("update", tabname, "set", blankme, "=NULL where", idcolname, "=", id, sep=" "))
    dbGetQuery(teaenv$con, paste("update", tabname, "set", blankme, "=NULL where", idcolname, "=", id, sep=" "))
}

#' Take in the name of a table in the database and an optional 'where' clause;
#' pull the subset specified, and check each row.
#' If a row fails, blank the element that fails the most edits (in case of ties,
#' randomly draw among the most failed), then call the imputation routine on that row.
EditTable <- function(tabname, where=NULL, do_preedits=1){
    if (!is.character(tabname)) {
        print("Give this function a name from the database.")
        return(NULL)
    }

    t <- teaTable(tabname, where=where)
    idcolname <- teaGetKey("id")
    idcol <- teaTable(tabname, cols=idcolname, where=where)
    fail <- TRUE
    autofill <- 1
    ctr <- 0 # Note also that the imputation makes 1,000 tries/record.
    while (fail == TRUE && ctr < 10) {
        fail <- FALSE
        t <- teaTable(tabname, where=where)
        glitches <- as.data.frame(.Call("RCheckData",t, as.integer(do_preedits)))
print(t)
print(glitches)

        for (i in 1:nrow(glitches)){
            r <- glitches[i,]
            if (sum(r)>0){
                fail <- TRUE
                blankOne(r, tabname, idcolname, idcol[[1]][i])
            }        
        }
        if (fail){
            .C("impute", as.character(tabname), as.integer(autofill))
        }
        #These are rowids where we couldn't draw a consistent record
        hardFails <- teaTable("tea_fails")
        fail <- !is.null(hardFails)
        ctr <- ctr+1
    }
    if (ctr == 10) warning("Some rows couldn't be edited into consistency.")
}
