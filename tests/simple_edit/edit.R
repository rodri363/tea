library(tea)

DF <- data.frame(id=1:6,age=c(1,2,NA,1,2,NA))
con <- dbConnect(dbDriver("SQLite"),"db")
dbGetQuery(con,"drop table if exists edit")
dbGetQuery(con,"create table edit (id integer, age integer)")
dbGetQuery(con,"begin transaction")
dbGetPreparedQuery(con,"insert into edit values ($id,$age)",DF)
dbGetQuery(con,"commit")

read_spec("spec")
DF <- dbGetQuery(con,"select * from edit")
vcheck <- CheckDF(DF,con)
ucheck <- .Call("r_check_a_table",DF)$Vector
