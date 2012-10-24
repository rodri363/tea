f<- file("spec")
writeLines(c( "database:t.db", 
            "input { input file: indata", 
			"overwrite: yes",
            "output table: data}",
            "fields { ",
			"age: int 1-100 ",
            "sex: cat m, f }",
            "#checks { age > 50 and sex =\'f\'",
            "#age <50 and sex=\'m\'}",
            "checks { age > 50 and sex =1",
            "age <50 and sex=1}",
            "impute{ input table: data",
            "models{",
            "age { method: normal }" ,
            "sex { method: hot deck }",
            "}}"
            ), f)
close(f)

g<-file("indata")
writeLines(c( "age|sex",
            "23|m",
            "83|m",
            "|NaN",
            "3|f",
            "83|f",
            "|f",
            "83|NaN"), g)
close(g)

db<-file("t.db")
unlink(db)
library(tea)
readSpec("spec")
doInput()
stopifnot(dbGetQuery(teaenv$con, "select count(*) from data where age is null") == 2)
stopifnot(dbGetQuery(teaenv$con, "select count(*) from data where sex is null") == 2)
doMImpute()

unlink(f)
unlink(db)
unlink(g)
