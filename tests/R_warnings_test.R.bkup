library(tea)
library(ggplot2)

warning_list <- list()
error_list <- list()

withCallingHandlers({

# Execute function:
readSpec("demo.spec")

# For any warnings, leave the scope of readSpec(...) and 
# execute the following warning function:
    }, warning=function(war) {
        print(paste("Current warning in readSpec(...): ", war))
        warning_list <- c(warning_list, war)

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
        print(paste("The error: ",err, " occurred when it shouldn't have."))
        error_list <- c(error_list, err)
    })

stopifnot(length(warning_list) == 0)


withCallingHandlers({
       doMImpute()

    }, warning=function(war) {
       print(paste("Current warning in doMImpute(): ",war))
       warning_list <- c(warning_list, war)

    }, error=function(err) {
       print(paste("The error: ", err, "occurred when it shouldn't have."))
       error_list <- c(error_list, err)

    })
