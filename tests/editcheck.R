library(tea)
readSpec("editcheck.spec")
doInput(do_preedit='no')
tab <- teaTable("ed")
tab2 <- CheckDF(tab)

#y1==1, weights/y1>30
stopifnot(tab2[1, "y1"] == 2)
stopifnot(tab2[1, "y2"] == 0)
stopifnot(tab2[1, "weights"] == 1)

#y1==1, sum is odd, w/y1>30
stopifnot(tab2[2, "y1"] == 3)
stopifnot(tab2[2, "y2"] == 1)
stopifnot(tab2[2, "weights"] == 1)

#sum is odd, unordered, w/y1>30
stopifnot(tab2[3, "y1"] == 3)
stopifnot(tab2[3, "y2"] == 2)
stopifnot(tab2[3, "weights"] == 1)

#y2*weights > 100, w/y1>30
stopifnot(tab2[4, "y1"] == 1)
stopifnot(tab2[4, "y2"] == 1)
stopifnot(tab2[4, "weights"] == 2)

#y1==1, but y2 is NaN, so only y1 checked.
stopifnot(tab2[5, "y1"] == 1)
stopifnot(tab2[5, "y2"] == 0)
stopifnot(tab2[5, "weights"] == 0)

#OK
stopifnot(tab2[6, "y1"] == 0)
stopifnot(tab2[6, "y2"] == 0)
stopifnot(tab2[6, "weights"] == 0)

#y2*weights > 100
stopifnot(tab2[8, "y1"] == 0)
stopifnot(tab2[8, "y2"] == 1)
stopifnot(tab2[8, "weights"] == 1)

EditTable("ed")
tab <- teaTable("ed")
tab3 <- CheckDF(tab)
