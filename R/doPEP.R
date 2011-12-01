
#' The preedits are reduced to a list of queries. We can apply them at any time to any
#' table to make sure that they are still clean.
#'
#' @param input_table The table to act on. If NULL, I'll check the active table
doPreedits <- function(input_table=pepenv$active_tab){
    .C("do_preedits", as.character(input_table))
}

#\key {input/input file} Path to a comma-separated value (CSV) file containing
#data to read into the database.
#\key {input/output table} Name for the database table generated from the input file
#\key input/overwrite Numeric key indicating whether the output table should be overwritten
#if it exists. A key value that is any capitalization of 'no' or 'n', or 0, will prevent overwriting, otherwise
#the default is to overwrite
#\key{input/primary key} A list of variables to use as the primary key for the output table.
#In SQLite, if there is only one variable in the list as it is defined as an integer,
#this will create an integer primary key and will thus be identical to the auto-generated
#ROWID variable.
#\key input/types A list of \emph{keys} of the form:
#var: type
#where var is the name of a variable (column) in the output table and type is a valid
#database type or affinity.  The default is to read in all variables as character
#columns.

#' Read a text file into a database table
#' @param input_file The name of the text file to read in. Make sure the path is
#'      correct and you have permissions to read the file.
#' @param output_table The name of the table to write to the database
#' @param overwrite If the key is present and "yes", then I will delete and re-insert 
#'		the table if need be. Otherwise, I will not overwrite existing data,
#'		meaning that you can re-run a script with one spec file and only wait for the
#'		read-in once. [Technically: if present and equal to a case-insensitive version
#'		of No, N, or 0, I won't overwrite; if present and anything else, overwrite.]
doInput <- function(input_file=NULL,output_table=NULL,types=NULL,primary.key=NULL,indices=NULL){
	if(is.null(pepenv$con)) stop("You need to have a config file read in!")
	con <- pepenv$con #for the attach-averse
    if (pepenv$verbosity > 0)
        print(dbGetQuery(con,"select * from keys"))
	if(is.null(input_file)) input_file <- PEPGetKey("input","input file")
	if(is.null(input_file)) stop("I couldn't find the \"input file\" key in the \"input\" section of the spec.")

	.C("text_in")
	tbl <- PEPGetKey("input", "output table")
	if(is.null(tbl)) 
		stop("I need an 'output table' name in the input section of the spec.")

    #Pre-edits are here for now, at read-in, on the raw data table.
    doPreedits(tbl)
	if (dbExistsTable(pepenv$con, paste("view",tbl,sep=""))){
		pepenv$active_tab <- paste("view",tbl,sep="")
	} else {
		pepenv$active_tab <- tbl
    }
	dbGetQuery(con,
		paste("drop table if exists",paste("ORIG",tbl,sep="")))
	dbGetQuery(con,
		paste("create table",paste("ORIG",tbl,sep=""),
			"as select * from", pepenv$active_tab))
}

doFingerprint <- function(flag.key=NULL,frequency=1,combinations=1,id=NULL,geolist=NULL,
input_table=NULL){
	if(is.null(pepenv$con)) stop("You need to have a config file read in!")
	con <- pepenv$con
	viewtbl <- getInputTable("fingerprint", input_table);
	if(is.null(flag.key)) flag.key <- PEPGetKey("fingerprint","key")
	if(is.null(flag.key)) warning("No fingerprint key for flagging, so flagging everyone!")
	if(is.null(geolist)) geolist <- list(PEPGetKey("fingerprint","geo"))
	id <- PEPGetKey("input","primary key")
	if(is.null(flag.key) | length(flag.key)==0){
		Rflag.SQL(con,viewtbl,NULL,0,id=id,geolist=geolist,all=TRUE)
	} else {
		frequency <- PEPGetKey("fingerprint","frequency")
		combinations <- PEPGetKey("fingerprint","combinations")
		Rflag.SQL(con,viewtbl,flag.key,frequency,combthresh=combinations,
			id=id,geolist=geolist,vflagrtn=TRUE,verbose=TRUE);
	}
	pepenv$overlay="vflags"
}

doMImpute <- function(verbose=0, seed=NULL, input_table=NULL

#    apop_data *category_matrix = get_key_text(configbase, "categories");
#    int min_group_size = get_key_float(configbase, "min_group_size");
#    int iteration_count = get_key_float(configbase, "draw_count");
#    //char *idatatab = get_key_text(configbase, "datatab", datatab)->text[0][0];
#	apop_data *dtab = get_key_text(configbase, "datatab");
#    //int verbose = get_key_float(configbase, "verbose", .default_val = 0);
#    int seed = get_key_float(configbase, "seed");
#    apop_data *impute_config = get_key_text(configbase, "imputes");
#	apop_data *dtab = get_key_text(configbase, "datatab");


){ 
	getInputTable("impute", input_table)
	print ("Imputing missing values")
 	.C("impute") 
}

pepenv <- new.env()
