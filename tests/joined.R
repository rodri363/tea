library(tea)
readSpec("joined.spec")
doTable("dc_united")

source("pokeHoles.R")
pokeHoles("dc_united", "pincp", .3)

doMImpute()

# Check out an imputation using the chosen method
# Query the absolute difference between the correct PINCP and the imputed
# Get log(diff+10), where the +10 deals with zeros
# Print a summary of the log differences.
getDiffs <- function(method, name){
    filltab <- paste("via_", method, sep="")
    outtab <- paste("complete_", filltab, sep="")
    checkOutImpute(origin="dc_united", dest=outtab, filltab=filltab)
    diffs <- dbGetQuery(teaenv$con, paste("select abs(dcu.pincp - imp.pincp) \
                       from precut dcu, ", outtab, "imp\
                       where dcu.id+0.0 = imp.id \
                       and dcu.agep>15 \
                       and dcu.id in (select id from dc_united where pincp is null)"))
    diffs <- log(diffs+10)
    print(paste("Log diffs for income imputed via", name))
    print(summary(diffs))
}

getDiffs("em", "EM algorithm (w/aux data)")
getDiffs("hot_deck", "Hot deck")
