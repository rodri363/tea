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
va <- as.integer(CheckDF(DF,con))
vb <- as.integer(.Call("r_check_a_table",DF)$Vector)
vc <- as.integer(c(0,1,1,0,1,1))

if(!identical(va,vc) | !identical(vb,vc)) stop("Error in consistency checking")
