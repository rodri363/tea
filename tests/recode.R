library(tea)
readSpec("recode.spec")
doInput()

tt <- teaTable("viewdc", limit=20, cols=c("id", "PINCP", "log_in",
                                          "hh_in", "house_in"))
print(tt)


print(summary(teaTable(teaenv$active_tab, cols=c("log_in", "log_house_in"),
                                     where="log_in+log_house_in > -100")))

print(dbGetQuery(teaenv$con, "select avg(log_house_in) from \
                                (select distinct serialno, log_house_in \
                                from viewdc where log_house_in > -100)"))
