library(tea)
library(ggplot2)

warning_list <- list()
error_list <- list()

tryCatch({

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
        print(paste("Darn! The error: ",err, " occurred when it shouldn't have!"))
        error_list <- c(error_list, err)

# After readSpec(...) is complete, execute the 
# following finally { ... } block:
    }, finally = {
        print("Exiting readSpec()")
    })

tryCatch({
       doMImpute()

    }, warning=function(war) {
       print(paste("Current warning in doMImpute(): ",war))
       warning_list <- c(warning_list, war)

    }, error=function(err) {
       print(paste("Uh oh, we've received the following error: ", err, ". Check your ",
                   "privelege...the laws of computing apply to you too."))
       error_list <- c(error_list, err)

    }, finally = {
       print("Exiting doMImpute()")
    })
