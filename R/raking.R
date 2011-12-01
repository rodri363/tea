#' Raking, aka Iterative Proportional Filtering
#'
#' @param all_vars  Semicolon-delimited list of all of the variables to be
#'   considered. Either one variable per line or semicolon-delimited list, like <tt>blockid; sex; age;</tt> Default: None.
#'
#   @group raking
#'  @param table	The name of the starting table.
#'  @param run_number  If call this function simultaneously over multiple threads, give each a different value here. Default: set by the system.
#'  @param all_vars     This list of all variables in the table for me to consider,
#'      separated| by |pipes. Columns not listed here are entirely ignored. If you 
#'      leave this as NULL, then I will use all the columns in the given table.  Default: NULL.
#'  @param contrasts    The contrasts. Everything along here will be held constant. I think
#'    the documentation for all this exists somewhere; I've gotta find it.  Default: NULL.
##%        blockid |qsex |qageshort |racep
##%        blockid |qsex |qageshort |qspanq
##%        qsex |qageshort |racep |qspanq 
##%\item{table}
#'  @param  tolerance   If the total change from one step to the next is less than this, I stop.    Default: 1e-5.
#'  @param  max_iterations After this many steps, I give up    Default: 1000.
#'  @param  thread_count   How many processor threads should I use internally? Default: 1.
#'  @param  run_number I put all the data in one table; use this integer to prevent
#'          overwriting and to retrieve results.

doRake <- function(table=NULL, run_number=1, all_vars=NULL, contrasts=NULL, tolerance=1e-5, 
                                            max_iterations=1000, count_col=NULL, thread_count = 1, init_table=NULL, init_count_col=NULL, nudge=0){
    set_key("raking", "all vars", all_vars)
    set_key("raking", "contrasts", contrasts)
    set_key("raking", "tolerance", tolerance)
    set_key("raking", "max iterations", max_iterations)
    set_key("raking", "count col", count_col)
    set_key("raking", "thread count", thread_count)
    set_key("raking", "init table", init_table)
    set_key("raking", "init count col", init_count_col)
    set_key("raking", "nudge", nudge)
	getInputTable("raking");
    .Call("rake_for_r")

	pepenv$active_tab <- paste(PEPGetKey("raking/active_table")[[1]], "_raked", sep="")
    return(dbGetQuery(pepenv$con, paste("select * from ", pepenv$active_tab)))
}



#I think this is only used above.
set_key <- function (group, key, indata){
    if (!is.null(indata)) {
        .C("set_key_text_for_R", as.character(group), as.character(key),  as.character(indata))
    }
}
