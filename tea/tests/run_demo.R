if (FALSE){
setwd("../demo")
source("demo.R")


sexlist <- teaTable("viewdc", col="sex")
stopifnot(length(sexlist[sexlist=="1"]) + length(sexlist[sexlist=="2"]) 
                == length(unlist(sexlist))
        )


#agelist <- sapply(teaTable("viewdc", col="agep"),as.numeric)
agelist <- checkOutImpute()
stopifnot(length(agelist[agelist<0 || agelist > 90]) > 0)
}
