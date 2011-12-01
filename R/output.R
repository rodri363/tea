summary.tea <-
	function (object, digits = max(3, .Options$digits - 3), ...)
{
    cat("this is a tea summary!")
}

########################################################################
	
pums <- function (x, var.name.truncate = 20, type.truncate = 14) 
{
    if (!is.data.frame(x)) {
        x <- data.frame(x)
        warning("Object coerced to a data frame.\n")
    }
    if (is.na(length(dim(x))) | is.null(length(dim(x)))) 
        stop("You can not be serious!\n")
    sum.na <- function(y) sum(y, na.rm = TRUE)
    size <- dim(x)
    is.fac <- unlist(lapply(x, is.factor))
    is.char <- unlist(lapply(lapply(x, as.vector), is.character))
    is.fc <- is.fac | is.char
    is.ord <- unlist(lapply(x, is.ordered))
    sum.var.na <- apply(is.na(x), 2, sum)
    is.mixed <- rep(TRUE, size[2])
    this.type <- rep("", size[2])
    num.values <- rep(0, size[2])
    summ <- matrix(NA, size[2], 2)
    summ <- as.data.frame(summ)
    names(summ) <- c("min", "max")
    precision <- rep(NA, size[2])
    var.abbrev <- rep("", size[2])
    for (i in 1:(size[2])) {
        if (nchar(names(x)[i]) > var.name.truncate) {
            var.abbrev[i] <- paste(substr(names(x)[i], 1, var.name.truncate), 
                "&", sep = "")
        }
        else var.abbrev[i] <- names(x)[i]
        num.values[i] <- length(table(x[, i]))
        z <- as.character(x[, i])
        has.nonnumeric <- (regexpr("[^0-9.-]", gsub(" ", "", 
            z), perl = TRUE) > 0)
        has.numeric <- (regexpr("[0-9.-]", gsub(" ", "", z), 
            perl = TRUE) > 0)
        is.mixed[i] <- sum.na(has.nonnumeric) > 0 & sum.na(has.numeric) > 
            0
        if (is.fac[i] & !is.mixed[i]) 
            this.type[i] <- "pure factor"
        if (is.fac[i] & is.mixed[i]) 
            this.type[i] <- "mixed factor"
        if (is.ord[i]) 
            this.type[i] <- "ordered factor"
        if (is.char[i] & !is.fac[i]) 
            this.type[i] <- "character"
        if (!is.char[i] & !is.fac[i]) 
            this.type[i] <- "numeric"
        xtemp <- x[!is.na(x[, i]), i]
        if (length(xtemp) == 0) {
            summ[i, ] <- rep(NA, 2)
            precision[i] <- NA
        }
        else {
            if (this.type[i] == "numeric") {
                z <- as.character(x[, i])
                has.decimal <- as.vector(regexpr(".", z, fixed = TRUE))
                has.decimal[is.na(has.decimal)] <- 0
                has.decimal[has.decimal < 0] <- 0
                has.decimal[has.decimal > 0] <- nchar(z[has.decimal > 
                  0]) - has.decimal[has.decimal > 0]
                precision[i] <- 10^(-1 * max(has.decimal[!is.na(has.decimal)]))
            }
            if (this.type[i] == "numeric") 
                summ[i, ] <- c(min(x[, i], na.rm = TRUE), max(x[, 
                  i], na.rm = TRUE))
            if (this.type[i] != "numeric") 
                summ[i, ] <- sort(as.character(x[, i]))[c(1, 
                  length(xtemp))]
        }
    }
    summary.by.variable <- data.frame(variable.name = var.abbrev, 
        type = substring(this.type, 1, type.truncate), missing = sum.var.na, 
        distinct.values = num.values, precision = precision)
    summary.by.variable <- cbind(summary.by.variable, summ)
    row.names(summary.by.variable) <- 1:ncol(x)
    return(summary.by.variable)
}

get.table <- function(table) {
#    print (dbGetQuery(con, c("select * from ", table)))
    .C("show_db_table", as.character(table));
}

get.means <- function(table) {
    .C("get_means");
}
