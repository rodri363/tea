
#' The preedits are reduced to a list of queries. We can apply them at any time to any
#' table to make sure that they are still clean.
#'
#' @param input_table The table to act on. If NULL, I'll check the active table
doPreedits <- function(input_table=teaenv$active_tab){
    .C("do_preedits", as.character(input_table))
}

doRecodes <- function(tag=NULL){
    .C("make_recode_view", as.character(tag));
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
doInput <- function(input_file=NULL,output_table=NULL,types=NULL,primary.key=NULL,indices=NULL,overwrite=NULL){
	if(is.null(teaenv$con)) stop("You need to have a spec file read in!")
	con <- teaenv$con #for the attach-averse
    if (teaenv$verbosity > 0)
        print(dbGetQuery(con,"select * from keys"))
	if(is.null(input_file)) input_file <- teaGetKey("input","input file")
	if(is.null(input_file)) stop("I couldn't find the \"input file\" key in the \"input\" section of the spec.")

	if(is.null(overwrite)) overwrite <- teaGetKey("input","overwrite")
	if(overwrite!="yes") return("No overwrite");
	tbl <- teaGetKey("input", "output table")
    print(paste("Reading text file '", input_file, "' into output table '", tbl, "'."))
	.C("text_in")
	if(is.null(tbl)) stop("I need an 'output table' name in the input section of the spec.")

    #Pre-edits are here for now, at read-in, on the raw data table.
    doPreedits(tbl)
	if (dbExistsTable(teaenv$con, paste("view",tbl,sep=""))){
		teaenv$active_tab <- paste("view",tbl,sep="")
	} else {
		teaenv$active_tab <- tbl
    }
	dbGetQuery(con,
		paste("drop table if exists",paste("ORIG",tbl,sep="")))
	dbGetQuery(con,
		paste("create table",paste("ORIG",tbl,sep=""),
			"as select * from", teaenv$active_tab))
}

doFingerprint <- function(flag.key=NULL,frequency=1,combinations=1,id=NULL,geolist=NULL,
input_table=NULL){
	if(is.null(teaenv$con)) stop("You need to have a spec file read in!")
	con <- teaenv$con
	viewtbl <- getInputTable("fingerprint", input_table);
	if(is.null(flag.key)) flag.key <- teaGetKey("fingerprint","key")
	if(is.null(flag.key)) warning("No fingerprint key for flagging, so flagging everyone!")
	if(is.null(geolist)) geolist <- list(teaGetKey("fingerprint","geo"))
	id <- teaGetKey("input","primary key")
	if(is.null(flag.key) | length(flag.key)==0){
		Rflag.SQL(con,viewtbl,NULL,0,id=id,geolist=geolist,all=TRUE)
	} else {
		frequency <- teaGetKey("fingerprint","frequency")
		combinations <- teaGetKey("fingerprint","combinations")
		Rflag.SQL(con,viewtbl,flag.key,frequency,combthresh=combinations,
			id=id,geolist=geolist,vflagrtn=TRUE,verbose=TRUE);
	}
	teaenv$overlay="vflags"
}

doMImpute <- function(tag=NULL, input_table=teaenv$active_tab){ 
#    apop_data *category_matrix = get_key_text(configbase, "categories");
#    int min_group_size = get_key_float(configbase, "min_group_size");
#    int iteration_count = get_key_float(configbase, "draw_count");
#    //char *idatatab = get_key_text(configbase, "datatab", datatab)->text[0][0];
#	apop_data *dtab = get_key_text(configbase, "datatab");
#    //int verbose = get_key_float(configbase, "verbose", .default_val = 0);
#    int seed = get_key_float(configbase, "seed");
#    apop_data *impute_config = get_key_text(configbase, "imputes");
#	apop_data *dtab = get_key_text(configbase, "datatab");


	print ("Imputing missing values")
    active_tab <- getInputTable("impute")
    if (!is.null(tag)) print (paste("tag= ", tag))
    print (paste("input table= ", active_tab))

#    rmodel <- TEAGetKey("impute", "%%/Rmodel", tag)
#    mod <- NULL
#    if (!is.null(rmodel)){
#        mod <- get(rmodel)$model
#        #est <- estimateRapopModel(list(), mod)
#    }
    .C("impute", as.character(tag), as.character(active_tab)) 
	teaenv$active_tab <- active_tab #active_tab may have changed
}

teaenv <- new.env()
