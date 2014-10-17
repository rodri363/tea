library(tea)
readSpec("editcheck.spec")
doInput()
tab <- teaTable("ed")
tab2 <- CheckDF(tab)

#y1==1, otherwise pass.
stopifnot(tab2[0, "y1"] == 1)
stopifnot(tab2[0, "y2"] == 0)

#y1==1, sum is odd
stopifnot(tab2[1, "y1"] == 2)
stopifnot(tab2[1, "y2"] == 1)

#sum is odd, unordered
stopifnot(tab2[2, "y1"] == 2)
stopifnot(tab2[2, "y2"] == 2)

stopifnot(tab2[3, "y1"] == 0)
stopifnot(tab2[3, "y2"] == 0)

#y1==1, but y2 is NaN, so only y1 checked.
stopifnot(tab2[4, "y1"] == 1)
stopifnot(tab2[4, "y2"] == 0)
