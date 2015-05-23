library(tea)
readSpec("edit.spec")

# Because the data are already clean, let's insert some errors to edit.
dbGetQuery(teaenv$con, "update viewdc \
                    set pincp=1000 where agep = 12")
dbGetQuery(teaenv$con, "update viewdc \
                    set schl=22 where agep between 10 and 14 and id%2 == 0")

doEdit()
doMImpute()
