library(tea)
readSpec("SPEER.spec")

# low <- dbGetQuery(teaenv$con, "select lower from SPEERimpl where numer='FEMP' and denom='FEMP'")
# stopifnot(low==1)
