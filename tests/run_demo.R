if (FALSE){
setwd("../demo")
source("demo.R")


sexlist <- show_db_table("viewdc", col="sex")
stopifnot(length(sexlist[sexlist=="1"]) + length(sexlist[sexlist=="2"]) 
                == length(unlist(sexlist))
        )


#agelist <- sapply(show_db_table("viewdc", col="agep"),as.numeric)
agelist <- checkOutImpute()
stopifnot(length(agelist[agelist<0 || agelist > 90]) > 0)
}
