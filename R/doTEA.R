
#' The preedits are reduced to a list of queries. We can apply them at any time to any
#' table to make sure that they are still clean.
#'
#' @param input_table The table to act on. If NULL, I'll check the active table
doPreedits <- function(input_table=teaenv$active_tab){
    .C("do_preedits", as.character(input_table))
}

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
#if(overwrite!="yes") return("No overwrite");
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
	print ("Imputing missing values")
    active_tab <- getInputTable("impute")
    if (!is.null(tag)) print (paste("tag= ", tag))

#    rmodel <- TEAGetKey("impute", "%%/Rmodel", tag)
#    mod <- NULL
#    if (!is.null(rmodel)){
#        mod <- get(rmodel)$model
#        #est <- estimateRapopModel(list(), mod)
#    }
    if (!is.null(tag)){
        .C("do_impute", as.character(tag), as.character(active_tab)) 
    } else {
        .C("impute", as.character(active_tab)) 
    }
	teaenv$active_tab <- active_tab #active_tab may have changed
}

teaenv <- new.env()



#' Perform regression on a data set and generate
#' synthetic data conforming to consistency rules.
#'
#' @param model.spec a list of modeling options, specified as:
#' \enumerate{
#' \item [[*]][[1]] = an R formula (either as a formula object or a character),
#' giving the initial model to fit on the data.
#' \item [[*]][[2]] = a length 1 character containing the name of a supported regression function
#' \item [[*]][[3]] = a character vector containing the optional right-hand-side variables
#' }
#' Each [[*]] element represents a new LHS variable for modeling. The order of model
#' fitting is decided by the ordering of the list.
#' @param data a data frame containing, at a minimum, all variables found in model.spec
#' @ncore The number of threads to use in processing
#' @bayes A non-conformist reverend, living in the late 1700s.
#' @input_table As described.
#' @return "Regression Complete"
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}

doRegression <- function(model.spec=NULL,by=NULL,ncore=NULL,bayes=NULL, input_table=NULL,
	verbose=NULL){
	if(is.null(teaenv$con)) stop("You need to have a config file read in!")
	if(is.null(teaenv$overlay)) stop("You need to tell me which overlay to use!")

	all.vars <- character(0) #keep track of all variables needed to reduce data.frame size
	key1 <- "Regression"
	by.vars <- teaGetKey(key1,"by")
	all.vars <- c(all.vars,by.vars)
	if(is.null(ncore)) ncore <- teaGetKey(key1,"ncore")
	if(is.null(ncore)) ncore <- 1
	if(is.null(bayes)) bayes <- teaGetKey(key1,"bayes")
	if(!is.null(bayes)) bayes <- bayes > 0
	if(is.null(bayes)) bayes <- 0
	if(is.null(verbose)) verbose <- teaGetKey(key1,"verbose")
	if(is.null(verbose)) verbose <- FALSE
	if(!is.null(verbose)) verbose <- TRUE

	if(is.null(model.spec)){
		LHSs <- teaGetKey(key1,"models", is_sub=TRUE)
		if(is.null(LHSs)) stop("You need to have at least one model specification!!")
		all.vars <- c(all.vars,LHSs)
	
		model.spec <- vector("list",length(LHSs))
		names(model.spec) <- LHSs

		#using overlay, create 'flags' input for RegEditSyn
		flags <- vector("list",2)
		flags[[1]] <- dbGetQuery(teaenv$con,paste("select * from",teaenv$overlay))
		flags[[2]] <- vector("list",length(LHSs))
		names(flags[[2]]) <- LHSs

		consistency <- NULL
		edit.subsets <- list()
		checks <- list()


		for(lhs in LHSs){
			checks[[lhs]] <- NA
			key2 <- paste(key1,"models",lhs,sep="/")
			flags[[2]][[lhs]] <- teaGetKey(key2,"flag")
			edit.subset <- teaGetKey(key2,"edit_subset")
			check <- teaGetKey(key2,"check")
			if(length(check>0)) checks[[lhs]] <- check
			if(length(edit.subset>0)) edit.subsets[[lhs]] <- edit.subset

			predictors <- teaGetKey(key2,"predictors")
			if(!identical(predictors,"1")) all.vars <- c(all.vars,predictors)
			predictors <- paste(lhs,"~",paste(predictors,collapse="+"))
            model.spec[[lhs]] <- list(predictors,teaGetKey(key2,"model"))
		}
		print("Here are the checks")
		print(checks)
		if(length(checks) == length(LHSs)){
			consistency <- checks
			edit.vars <- dbGetQuery(teaenv$con,"select name from variables")[,"name"]
			if(length(edit.subset)>0) edit.vars <- edit.subset
			all.vars <- c(all.vars,edit.vars)
		}
	}
	#need to do all.vars set for the case when model.spec is specific in the parameters
	all.vars <- unique(all.vars)
	viewtbl <- getInputTable(key1, input_table)

	if(length(by.vars)>0){
		domains <- dbGetQuery(teaenv$con,paste("select distinct",paste(by.vars,collapse=",")," from",viewtbl))[by.vars]
		#want each list elements of domains to be a data frame with ONE combination of
		#the primary key variables
		domains <- split(domains,1:nrow(domains))
	}else{
		#will generate "where 1=1" in the by query below
		#will thus select all records
		by.vars <- "1"
		domains <- "1"
	}

	primary_key <- teaGetKey("input","primary_key")
	#domains <- lapply(domains,function(x) return(list(domain=x,con=dbConnect(dbDriver("SQLite"),dbGetInfo(teaenv$con)$dbname))))

	by.f <- function(domain){
		con.tmp <- dbConnect(dbDriver("SQLite"),dbGetInfo(teaenv$con)$dbname)
		print(paste("Domain:",paste(domain,collapse=",")))
        #BK hack to deal with still more text/numeric issues:
        domain[,] <- paste('"' , domain[,] , '"', sep="")
		#query to select data in domain for all the variables needed for modeling
		query <- paste("select",paste(c(primary_key,all.vars),collapse=","),
					"from",viewtbl,"where",
					paste(by.vars,domain,sep="=",collapse=" and "));
		data <- try(dbGetQuery(con.tmp,query))
		if(inherits(data,"try-error")) print(data)
		syn <- try(RegEditSyn(model.spec,data,flags,id=primary_key,
			consistency=consistency, bayes=bayes,ncore=1,verbose=verbose),silent=TRUE)
#		syn <- try(RegEditSyn(model.spec,data,flags,id=primary_key,
#			consistency=consistency, edit.subsets=edit.subsets, bayes=bayes,ncore=1,verbose=verbose),silent=TRUE)
		if(inherits(syn,"try-error")){
			print(paste("Domain",paste(domain,collapse=","),"didn't work for synthesis"))
			print(syn)
			return(data)
		}else{
			return(syn)
		}
		dbDisconnect(con.tmp)
	}
	updates <- lapply(domains,by.f)
	
	save(updates,file="updates.RData")
	lapply(updates,UpdateTablefromDF,tbl=teaGetKey("input", "output table"),
		con=teaenv$con,cols=LHSs,match.key=primary_key,
		ncommit=10000,verbose=TRUE)
	teaenv$active_tab <- viewtbl
	return("Regression Complete")
}
