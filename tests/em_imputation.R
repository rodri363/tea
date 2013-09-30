diff <- function(a, b, eps){
    stopifnot(abs(a-b) < eps)
}


library(tea)
readSpec("em_imputation.spec")
doInput()
doMImpute()


#The EMed  weights should look like:
#y1  y2   Weights
# 1   1  0.279418
# 1   2  0.174151
# 2   1  0.238826
# 2   2  0.307606


id <- dbGetQuery(teaenv$con, "select id from t where y1=1 and y2 is null")
qv1 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=1 and id+0.0=", id))
qv2 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=2 and id+0.0=", id))
diff(qv1/qv2, 0.279418/0.174151, 1e-1)

id <- dbGetQuery(teaenv$con, "select id from t where y1=2 and y2 is null")
qv1 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=1 and id+0.0=", id))
qv2 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=2 and id+0.0=", id))
diff(qv1/qv2, 0.2388/0.3076, 1e-1)

id <- dbGetQuery(teaenv$con, "select id from t where y2=1 and y1 is null")
qv1 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=1 and id+0.0=", id))
qv2 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=2 and id+0.0=", id))
diff(qv1/qv2, 0.279418/0.238826, 1e-1)


id <- dbGetQuery(teaenv$con, "select id from t where y2=2 and y1 is null")
qv1 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=1 and id+0.0=", id))
qv2 <- dbGetQuery(teaenv$con, paste("select count(*) from i where value+0.0=2 and id+0.0=", id))
diff(qv1/qv2, 0.174151/0.307606, 1e-1)

