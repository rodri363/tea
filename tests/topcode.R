library(tea)
readSpec("topcode.spec")
doInput()

print(summary(teaTable(teaenv$active_tab, cols=c("hh_in"))))

doEdit()

print(dbGetQuery(teaenv$con, "select avg(log_house_in) from \
                                        (select distinct serialno, log_house_in \
                                         from viewdc where log_house_in > -100)"))
