#' Interpolated median
#' @param x real/numeric vector
#' @return The interpolated median of x
imedian <- function(x){
	return(
		ifelse(sum(x==median(x))==0,
				median(x),
				median(x) - (1/2) + 
				((1/2)*length(x)-sum(x<median(x)))/sum(x==median(x))));
}

#' Pretty quoting for PEP
#' @param x character string to be pretty quoted
#' @return "'x'"
pQuote <- function(x){
	wch <- unlist(lapply(x,is.numeric))
	rstr <- character(length(x))
	rstr[wch] <- as.character(x[wch])
	rstr[!wch] <- paste("'",as.matrix(x[!wch]),"'",sep="")
	return(rstr)
}

#' Write an R data frame to an SQLite table
#' @param data the data frame to write
#' @param tbl the name of the SQLite table to write
#' @param con a database connection
#' @param var.def a character matrix, each row of which forms a pair
#' c("var","type"), where var is a variable name
#' and type is an SQLite datatype (integer, real,
#' text). Any variable in the data frame and NOT
#' listed in var.def will be converted as text.
#' @param primary.key variables to use for primary key. By default, NULL.
#' Note that if you have only one variable in primary.key
#' and that variable is given in var.def as an 'integer',
#' then that variable will become the integer primary key.
#' @param new.vars character vector of new variables to add
#' Types for these variables should be given in var.def as necessary
#' @param overwrite overwrite existing table?
#' @param verbose verbose output (create and insert statements)
#' @param ncommit number of observations to write per commit
#' @TODO add support for empty vars, recodes(triggers), indices
#' and foreign keys

WriteTable <- function(data,tbl,con,var.def=NULL,primary.key=NULL,
	new.vars=NULL,overwrite=FALSE,verbose=FALSE, ncommit=10000){

	if(dbExistsTable(con,tbl) & !overwrite) stop("Table exists and overwrite not allowed!")
	if(overwrite) dbGetQuery(con,paste("drop table if exists",tbl))
	if(ncol(data[primary.key])>0) if(any(is.na(data[primary.key]))) stop("Primary key variables have missing values!")

	all.vars <- c(names(data),new.vars)
	#types <- rep("text",length(names(data)))
	types <- rep("text",length(all.vars))
    if (!is.null(var.def)){
        for(ddx in 1:nrow(var.def)){
            if(is.null(var.def[ddx,2]) | length(var.def[ddx,2])==0) stop("Bad variable definition")
            vardx <- which(all.vars==var.def[ddx,1])
            types[vardx] <- var.def[ddx,2]
        }
    }
	#query <- paste("create table",tbl,"(",paste(names(data),types,sep=" ",collapse=", "),
	#CREATE TABLE
	query <- paste("create table",tbl,"(",paste(all.vars,types,sep=" ",collapse=", "),
		ifelse(is.null(primary.key),")",paste(", primary key","(",paste(primary.key,collapse=","),") )")));
	if(verbose) print(query)
	dbGetQuery(con,query)
	time <- proc.time()

	#INSERT
	new.vars.string <- ifelse(length(new.vars)>0,
		paste(",",paste(rep("null",length(new.vars)),collapse=","),sep=""),
		"")
	query <- paste("insert into",tbl,"values (",paste(":",names(data),sep="",collapse=", "),
		new.vars.string,")")
	if(verbose) print(query)
	first.row <- 1
	last.row <- min(c(first.row+(ncommit-1),nrow(data)))
	while(first.row <= nrow(data)){
		print(paste("Committing rows",first.row,"to",last.row))
		dbGetQuery(con,"begin transaction")
		dbGetPreparedQuery(con, query, bind.data=data[first.row:last.row,])
		dbGetQuery(con,"commit")
		first.row <- last.row+1
		last.row <- min(c(first.row+(ncommit-1),nrow(data)))
	}
	
	print("Time taken:")
	print(proc.time() - time)
}

#' Update an SQL table from an R data frame
#' @param data the data frame
#' @param tbl the name of the SQLite table to update
#' @param con a database connection
#' @param cols character vector of column names to update
#' @param match.key character vector of column names on which to match
UpdateTablefromDF <- function(data,tbl,con,cols,match.key,ncommit=10000,verbose=FALSE){
	if(!(all(c(cols,match.key) %in% names(data)))){
		stop(paste("Variables",paste(setdiff(c(cols,match.key),names(data)),collapse=","),
			"not found in data frame"));
	}
	query <- paste("update",tbl,"set",
		paste(cols,paste(":",cols,sep=""),sep="=",collapse=","),
		"where",
		paste(match.key,paste(":",match.key,sep=""),sep="=",collapse=" and "))
    if(verbose) print(query)
	first.row <- 1
	last.row <- min(c(first.row+(ncommit-1),nrow(data)))
	while(first.row <= nrow(data)){
		if(verbose) print(paste("Committing rows",first.row,"to",last.row))
		t0 <- proc.time()
		dbGetQuery(con,"begin transaction")
		dbGetPreparedQuery(con, query, bind.data=data.frame(data[first.row:last.row,],stringsAsFactors=FALSE))
		#for(rdx in first.row:last.row){
		#	query <- paste("update",tbl,"set",
		#		paste(cols,pQuote(data[rdx,cols]),sep="=",collapse=","),
		#		"where",
		#		paste(match.key,pQuote(data[rdx,match.key]),sep="=",collapse=" and "))
		#	dbGetQuery(con,query)
		#}
		dbGetQuery(con,"commit")
		first.row <- last.row+1
		last.row <- min(c(first.row+(ncommit-1),nrow(data)))
		if(verbose) print(proc.time()-t0)
	}
}

#' Show a database table
#'
#' @param table The name of the table
#' @param cols The columns to show, comma separated, as in cols="age, race, sex". Default: all of them.
#' @param limit The maximum number of rows to show. If there are more rows than
#'     the screen can handle, you will be able to page through them. Default: all rows
#' @param offset The first row to show, should you want a slice from the
#'     middle of the table. Default: start at the first row of the table.
#' @param where Constraints on the columns. This option is so-named because
#'     anything that can go into an SQL where clause can go here. Default: no constraints.
#' @param show This function returns the table as an R data frame. If you want
#'     to use it for that without viewing the table, set show=FALSE. Default: show the table.
#' @param annotated If TRUE, write to a file named [your_table].html. That table will
#' check the tables of missing, edited, and at-disclosure-risk data, and color entries that
#' have been touched to address these issues. Until we work out how to do it
#' automatically, navigate to this file with your browser to view the results. If you use
#' this option, the table you request must have the column of row IDs.

#' @return The data is returned as an R table.
show_db_table <- function(table=NULL, cols=NULL, limit=NULL, offset=NULL, where=NULL, show = TRUE, html=NULL){
	if (is.null(table))  stop("You need to specify a table for me to show") 

	q <- paste("select ",
			ifelse(is.null(cols), " * ", cols),
			" from ", table,
			ifelse(is.null(where), " ", paste(" where ", where)),
			ifelse(is.null(limit), 
					ifelse (is.null(offset), " ", paste("limit select count(*) from ", table))
					, paste(" limit ", limit)),
			ifelse(is.null(offset), " ", paste(" offset ", offset))
		)
print(q)
	out <- as.data.frame(dbGetQuery(pepenv$con, q))
    if (!is.null(html))
        print_to_html(out, html)
#	if (show) page(out) 
	out
}



# You have three options on how to set tables. 
# Highest precedence: send an input_table as an argument to the R function
# Next: set the "input_table" setting in the relevant segment of the spec file. 
# default: the # pepenv$active_tab variable. The idea is that on output, a procedure 
# sets its last-edited table as the active_tab, and then the next step of the flow can
# pick it up. 

getInputTable <- function(segment, cmd_line_input=NULL){
	out <- cmd_line_input
	if (is.null(out))
		out <- PEPGetKey(segment, "input table")
	if (is.null(out))
		out <- pepenv$active_tab
	if (is.null(out)) 
		stop(paste("I can't find an \"input table\" setting in the", segment, "part of the spec file, and I currently don't have a default to use."))

	return(out)
}

rnorm.edit <- function(mean,sigma,alts){
	draws <- floor(rnorm(10000,mean,sigma))
	wch <- which(draws %in% alts)
	if(length(wch) > 0) return(draws[wch[1]])
	return(NULL)
}


#'fill dest with a complete data set, using origin for the main part of the data, and
#' filling in the missing elements with data from the imputation you just run (which populated 
#' the "filled" table). Give an imputation number, and if you want to pull just a subset of the origin, 
#' set subset to something to put into a "where" clause.

#' @param origin The main data set, with missing values
#' @param dest  If given and not NULL, the output table name. If NULL, fill dest.
#' @param imputation_number You had multiple imputations; which do you want? First is zero.
#' @param subset What to put into a 'where' clause if you want less than the whole table.

checkOutImpute <- function(origin=NULL, dest=NULL, imputation_number=0, subset=NULL) {
	.C("check_out_impute", as.character(origin), as.character(dest), 
					as.integer(imputation_number), as.character(subset))
}


# keyTOval, existKey, getKey, CSVtoTAB unused; removed.
# Restore via git checkout 6c8a929c .

#' Get key values using the underlying PEP database connection
#' @param group the key group
#' @param key the key
#' @return character vector containing all the lines from the key
PEPGetKey <- function(group, key=NULL){
	group <- as.character(group)
	sub <- FALSE
	if(is.null(key)) sub <- TRUE
	key <- as.character(key)
	sub <- as.integer(sub)
	out <- character(.C("get_key_count_for_R",group, key,integer(1),sub)[[3]])
	if(length(out)==0) return(NULL)
	vals <- .C("get_key_text_for_R",group,key,out,sub)[[3]]
	if(sub>0){
		#pull out text before "/" and find unique keys
		vals <- unique(unlist(lapply(strsplit(vals,"/"),"[[",1)))
	}
	return(vals)
}

#' Merge the value given as input to the function with the value looked up in 
#' the spec. The function input is more immediate and so takes precedence.
#' @param group The group in the spec that the key can be found in.
#' @param key    The name of the key.
#' @param invalue The value sent in by the user. 
#' @param usekey  Give the key precedence even if the invalue is not NULL, 
#' @param errormsg If the value is NULL at the end of this and there is an
#'   error message, then this variable is taken as mandatory. Stop execution 
#'   and print the message. If errormsg=NULL (the default), then if the 
#'   variable has no set value, return NULL.

TEAGetKey <- function(group="", key=NULL, invalue=NULL, errormsg=NULL, usekey=FALSE){
    outval <- eval(invalue)
    specvalue <- PEPGetKey(group, key)
    if ((is.null(invalue) || usekey) && !is.null(specvalue)) outval <- specvalue
    if (is.null(outval) && !is.null(errormsg))
        stop(errormsg)
    return(outval)
}

#' Make levels of two model matrices agree.
#' @param M2 model matrix you want to conform to M1
#' @param M1 base model matrix
#' @return A matrix, with same number/order of columns as M1 and same number/order of rows as M2.
#' Non-zero entries come from M2.
TEAConformMatrix <- function(M2,M1){
	if(is.null(colnames(M1)))	stop("M1 must have column names")
	if(is.null(colnames(M2)))	stop("M2 must have column names")
	Vnames <- colnames(M1)
	Vin <- match(Vnames,colnames(M2))
	M1 <- matrix(0,nrow=nrow(M2),ncol=length(Vnames))
	M1[,!is.na(Vin)] <- M2
	colnames(M1) <- Vnames
	return(M1)
}

#' Convert a character to a factor
#' param x a vector
#' return a vector.  If x was character or a factor vector, returns factor(x); otherwise returns x
ffactor <- function(x){
	if(is.character(x)) return(factor(x))
	if(is.factor(x)) return(factor(x))
	else return(x)
}
