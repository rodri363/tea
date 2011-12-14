#' Given a set of variables and a data table,
#' append a flag variable based on
#' the cross-tabulation of those variables 
#' back onto the table,
#' using the variable names to name
#' the resulting new column
#' 
#' @param con database connection
#' @param tbl table
#' @param vars variable names (char vector)a
#' @param freqthresh frequency threshold for flagging a cell
#' @return nothing
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}
createFlag <- function(con,tbl,vars,freqthresh=1,
				  id="rowid"){
	dbGetQuery(con,"begin transaction");
	dbGetQuery(con,"drop table if exists b");
	dbGetQuery(con,"drop table if exists tab");
	flagname <- paste(paste(vars,collapse="_"),"flag",sep="_");

	query <- paste("create index if not exists",
		paste(tbl,paste(vars,collapse="x"),sep=""),
		paste("on ",tbl,"(",paste(vars,collapse=","),")",sep=""));
	dbGetQuery(con,query);

	query <- paste("create table tab as select",
		"a.*, b.flag as",flagname,"from",tbl,"a",
		",(select distinct",paste(vars,collapse=","),
		",count(*) <=",freqthresh,"as flag from",tbl,"group by",
		paste(vars,collapse=","),") b where",
		paste(paste("a",vars,sep="."),"=",
			paste("b",vars,sep="."),collapse=" and "));
	dbGetQuery(con,query);
	dbGetQuery(con,paste("drop table",tbl));
	dbGetQuery(con,paste("alter table tab rename to",tbl));
	dbGetQuery(con,"commit transaction");
}

#' Function to determine records at risk of disclosure.
#' @param con a connection to a database
#' @param tbl name of a database table containing records for flagging
#' @param risknames names of variables in dataset to be used for flagging
#' @param freqthresh the frequency threshold for flagging a record at risk for each variable combination
#'              If a record has a frequency at or below this this threshold in a table,
#'              it is flagged for the combination associated with that table
#' @param combthresh the upper limit of dimensionality for multiway tables
#'              e.g. if there are 5 variables in risknames, but combthresh = 4,
#'              then only the 1-way to 4-way tables will be considered
#' @param simple indicates whether all possible 1-way to combthresh-way tables should be considered (simple=F),
#'          or if only the largest table should be considered (simple=T).
#' @param reduce remove redudant flags (that is, if a record is flagged on say (A,B,C)
#'				and (A,B,C,D), the flag on (A,B,C,D) would be removed
#' @param geolist character vector of nested geographies.  Often geographies are given in nested format
#'				so that county=2 might exists for several states.  In this case,
#'				we remove those variable combinations where nested geographies
#'				are by themselves (e.g. (ST,CTY,X) is valid but (CTY,X) is invalid).
#'				Currently, only one sequence of nesting is allowed (given as a character string)
#'				in descending order of geographic size.
#' @param vflagtrn return weighted "per-variable" flags. If we have variables A,B,C
#'				would return a data set with 3 columns A|B|C.  If a record
#'				were flagged on (A,B) and (A,C), the raw per-var. flags would look like:
#'				A|B|C
#'				2|1|1
#'				These flags are then weighted by 1/(k!), where k is the 
#'				number of variable in the flagging combination.  For instance,
#'				in the example all flagging combinations are of k=2, so divide by
#'				2! = 2, hence final flags are:
#'				A|B|C
#'				1|.5|.5
#'				This makes a difference when k is larger and nesting is more complicated
#' @param verbose give verbose output?
#' @param all just flag everyone?
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}
#' @note Make sure there are no NAs in any of the flagging variables

Rflag.SQL <- function(con, tbl, risknames, freqthresh,
                  combthresh=length(risknames),
				  id=NULL,
                  simple=FALSE,reduce=TRUE,
                  geolist=NULL,
				  vflagrtn=FALSE,vflag.name="vflags",
				  verbose=FALSE,
				  all=FALSE){
    id <- TEAGetKey("", "id", invalue=id, 
            errormsg="Flagging needs an ID variable; add an 'id' key to the spec after the database name.")
    geolist <- TEAGetKey("fingerprint", "geography list")
  #function to convert decimal to binary character
  #returns character strings with binary conversions
  #all strings are of the same length, so leading
  #    zeroes are appended as necessary
  intToBin <- function(y){
    itb <- function(x){
      if(x>0){
        return(paste(as.character(itb(floor(x/2))),x%%2,sep=""));
      }
    }
    
    r <- character(length(y));
    r[y==0] <- as.character(0);
    r[y!=0] <- sapply(y[y!=0],itb);
    r <- paste(lapply(sapply(max(nchar(r)) - nchar(r),rep,x="0"),paste,collapse=""),r,sep="");
    return(r);
  }
  
##   function to determine subsets of variable combinations
##   Will return T if the T elements of y are also T in x; false otherwise
##   Ex: flagsub(x=TFFTT, y = TFFTF) returns T
##       flagsub(x=TFFTT, y = TTFTF) returns F
##   The case when y is all F is handled by returning F.
##   This is necessary since R gives all(logical(0)) as T.
##   In the current application, this situation never occurs.
  flagsub <- function(x,y){             
    w <- which(y) %in% which(x);
    if(identical(w,logical(0))){
      return(FALSE);
    }else{
      return(all(w));
    }
  }

  newflagsub <- function(x,y,gmat){
    wx <- which(x);
    wy <- which(y);
    geosub <- as.matrix(gmat[wx,wy]);
    if(identical(geosub,logical(0))){
      return(FALSE);
    }else{
      #return(all(apply(geosub,1,any)) | all(wx %in% wy));
      #need matrix() cast to prevent problem when geosub has 1 row
      #reprents as a row matrix now, instead of a column vector
      #as it is without cast.
      return(all(apply(matrix(geosub,nrow=length(wx)),1,any)) | all(wx %in% wy));
    }
  }
  
  rn <- length(risknames);

##	make flags table in database, reassign tbl
	dbGetQuery(con,paste("drop table if exists flags"));
	dbGetQuery(con,paste("create table flags as select",
		paste(c(id,risknames),collapse=","),"from",tbl))
	datarows <- dbGetQuery(con,"select count(*) from flags")

	if(all){
		dbGetQuery(con,paste("drop table if exists",vflag.name));
  		query <- paste("create table",vflag.name,"as",
			"select",paste(id,collapse=","),
			", 1 as FLAG from flags");
  		dbGetQuery(con,query)
		return("All records flagged")
	}

  
##   Test for conditions for simple flagging procedure.
##   The condition combthresh!=length(risknames) is necessary
##   since simple flagging *always* computes the full table given risknames,
##   thus asking for a smaller combination threshold is contradictory.
  if(simple & combthresh!=length(risknames)){
    stop("Simple flagging is only valid with combthresh=length(risknames)");
  }

##   Simple flagging procedure
##   Computes complete table given risknames and flag all records
##   whose associate frequency in the table falls below the
##   frequency threshold (freqthresh).
##   A dataset is returned, containing the variable in risknames and
##   the simple flag for all records.

	if(simple & combthresh==length(risknames)){
		stop("need to add code to handle simple case!!")
		#createFlag(db,"flags",risknames,freqthresh);
	}	

##   Only those combinations containing a number of variables
##   less than the combination threshold (combthresh) will be kept
##   in the final listing.
##   Initially, the combinations are stored as character strings
##   e.g. "10011".  Those kept are converted to logical rows of a matrix
##   e.g. TRUE FALSE FALSE TRUE TRUE

  combs <- intToBin(1:(2^rn-1));
  combs <- matrix(as.logical(as.numeric(unlist(sapply(combs,strsplit,split="")))),ncol=rn,byrow=T)
  combs <- combs[order(rowSums(combs)),];
  attributes(combs)[["dimnames"]] <- list(1:nrow(combs),risknames);

	#trim geographies
  for(geo in geolist){
    if(length(geo)>1){
      geomat <- matrix(FALSE,length(geo),length(geo));
	  geomat <- lower.tri(geomat)
      diag(geomat) <- TRUE;
      geomat <- rbind(FALSE,geomat)
      keep <- NULL;
      for(i in 1:nrow(combs)){
        if(any(apply(geomat,1,identical,as.logical(combs[i,geo])))){
          keep <- c(keep,i);
        }
      }
      combs <- combs[keep,];
    }
  }

	#impose combthresh limit
	notgeowch <- which(!(risknames %in% unlist(geolist)))
	combs <- combs[which(apply(combs[,notgeowch],1,sum) < combthresh),]


  if(nrow(combs)*nrow(datarows) > .Machine$integer.max){
    print("Error: Flag matrix exceeds maximum allowable matrix size");
    stop();
  }
  
##   Create matrix describing subset/superset relationships of
##   the combinations kept in combs.  For each combination in combs,
##   generates a row in the matrix cntn, which indicates if that combination
##   is a subset of every other combination.
##   The diagonal of square matrix cntn is set to FALSE, enforcing strict subsets.

  print("Forming containment array");
  geomat <- array(F,dim=rep(rn,2));
  for(geo in geolist){
    for(gdx in 1:(length(geo)-1)){
      #geomat[geo[gdx],geo[gdx:length(geo)]] <- T;
      geomat[gdx,gdx:length(geo)] <- T;
    }
  }
  diag(geomat) <- T;
 
  cntn <- matrix(F,nrow=nrow(combs),ncol=nrow(combs));
  for(i in 1:nrow(combs)){
    cntn[i,] <- apply(combs,1,flagsub,y=combs[i,]);
    #cntn[,i] <- apply(combs,1,newflagsub,y=combs[i,],gmat=geomat);
  }

  diag(cntn) <- F;
  
  
##   Create matrix of per-record flags.
##   For each combination in combs, generate the multi-way
##   frequency table for those variables in the combination,
##   and flag records whose associated variable values have a
##   frequency less than freqthresh.

  print("Flagging");
	flagnames <- NULL;
  for(i in 1:nrow(combs)){
    rvars <- risknames[combs[i,]];
    if(verbose) print(rvars);
	createFlag(con,"flags",rvars,freqthresh);
    flagnames <- c(flagnames,
		paste(paste(rvars,collapse="_"),"flag",sep="_"));
  }
	#read flags into matrix
	flags <- as.matrix(dbGetQuery(con,
				paste("select",paste(flagnames,collapse=","),"from flags")));

##   Trim the flags matrix:
##   This is best understood via example
  
##   Consider 3 variables, 1 2 3
##   There are 7 possible combinations of them
##   1 2 3 12 13 23 123

##   The matrix cntn will be
##       1   2   3   12  13  23  123
##   1   F   F   F   T   T   F   T
##   2   F   F   F   T   F   T   T
##   3   F   F   F   F   T   T   T
##   12  F   F   F   F   F   F   T
##   13  F   F   F   F   F   F   T
##   23  F   F   F   F   F   F   T
##   123 F   F   F   F   F   F   F

##   Consider a row of "flag" (flag[i,])
##      1   2   3   12  13  23  123
##   i  T   F   F   T   T   F   T

##   Matrix multiplication will compute the dot product of flag[i,] by each column of cntn.
##   For column k in cntn, the dot product is the following sum
##   ncol(flag)
##   ---
##   \
##   /   flag[i,j] * cntn[j,k]
##   ---
##   j = 1

##   The element flag[i,j] * cntn[j,k] is 1 only when flag[i,j] = T and cntn[j,k] = T,
##   that is, when the record is flagged on combination j and combination j is a
##   subset of combination k.  Thus if an element of resulting vector (flag[i,] * cntn)
##   is greater than 0, that indicates that the corresponding T element of flag[i,] should
##   be changed to F.  So we have the following criterion
##   If flag[i,j] = T & ([flag[i,] * cntn][,j] > 0) = T -> flag[i,j] = F
##   otherwise, flag[i,j] remains unchanged.
##   This implies the following truth table (A = flag[i,j], B = [flag[i,] * cntn][,j] > 0)
##   A  B
##   T  T | F (if flagged and a subset, remove flag)
##   T  F | T (if flagged and not a subset, retain flag)
##   F  T | F (if not flagged, no change)
##   F  F | F (if not flagged, no change)
##   which is the truth table for A & !B
##   So we perform two steps
##   1. Matrix-multiply "flags" and cntn, and test for > 0
##   2. Take the result (trm) and perform flags & !trm
##   In the example
##   flag[i,] * cntn -> 0 0 0 1 1 0 3
##   0 0 0 1 1 0 3 > 0 -> F F F T T F T
##   (T F F T T F T) & !(F F F T T F T) -> T F F F F F F

  if(reduce){
    trm <- (flags %*% cntn)>0;
    flags <- flags & !trm;
  }

##   Determine per-variable flags.
##   Matrix-multiplication of flags by combs will yield the number
##   of occurences of each flagging variable within the flagged
##   combinations per record.

#	the following code weights the per-variable flags
#	by the inverse of the length of the variable combination
# 	in which the flag occurred.
#	e.g. if the original flagging variables are
#	A B C
#	then A, B, and C would each get a weight of 1/3
#	This weights up smaller combinations (which are riskier)

  if(vflagrtn){
		dbGetQuery(con,paste("drop table if exists",vflag.name));
		vflags <- as.data.frame(flags%*%(combs/factorial(rowSums(combs))));
		names(vflags) <- risknames;
		vflags$FLAG <- as.integer(rowSums(vflags[,risknames])>0);
		query <- paste("select",paste(id,collapse=","),"from flags")
		print(query)
		vflags[id] <- dbGetQuery(con,query)
		dbWriteTable(con,vflag.name,vflags,row.names=FALSE);
	}
  
  #make different flagging interface, with repeating rows for each record
	flong <- function(Vrow){
		Mcomb <- cbind(combs,FALSE)
		colnames(Mcomb) <- c(colnames(combs),"FLAG")
		Mcomb[which(Vrow),"FLAG"] <- TRUE
		return(t(Mcomb))
	}
	Mlong <- apply(flags,1,flong)
	Mlong <- matrix(Mlong,ncol=ncol(combs)+1,byrow=TRUE)
	colnames(Mlong) <- c(colnames(combs),"FLAG")
	query <- paste("select",paste(id,collapse=","),"from flags")
	DFid <- dbGetQuery(con,query)
	DFlong <- as.data.frame(Mlong)
	Mnames <- matrix(unlist(lapply(DFid,function(x) sapply(x,rep,nrow(combs)))),nrow=nrow(Mlong),ncol=length(id))
	DFnames <- as.data.frame(Mnames)
	DFlong <- cbind(DFnames,DFlong)
	names(DFlong) <- c(id,colnames(Mlong))
	Mtypes <- cbind(names(DFlong),c(rep("text",length(id)),rep("integer",ncol(combs)+1)))
	WriteTable(DFlong,paste("long",vflag.name,sep="_"),con,Mtypes,NULL,overwrite=TRUE)
	query <- paste("drop index if exists longflag")
	dbGetQuery(con,query)
	query <- paste("create index longflag on",paste("long",vflag.name,sep="_"),"(",id,")")
	dbGetQuery(con,query)

	print("flagging complete");
}
