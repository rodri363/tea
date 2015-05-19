library(tea)
readSpec("preeditcheck.spec")
doInput()
tab <- teaTable("ed")
tab2 <- CheckDF(tab, do_preedits=0)
retab <- teaTable("ed")

stopifnot(retab[1, "y1"] == 4)
stopifnot(retab[2, "y2"] == 1)

doEdit()
tab <- teaTable("ed")
tab3 <- CheckDF(tab)
stopifnot(sum(tab3)==0)
