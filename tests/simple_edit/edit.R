library(tea)

DF <- data.frame(id=1:6,age=c(1,2,NA,1,2,NA),sex=as.character(c(1,2,2,1,NA,1)))
con <- dbConnect(dbDriver("SQLite"),"db")
dbGetQuery(con,"drop table if exists edit")
dbGetQuery(con,"create table edit (id integer, age integer, sex text)")
dbGetQuery(con,"begin transaction")
dbGetPreparedQuery(con,"insert into edit values ($id,$age,$sex)",DF)
dbGetQuery(con,"commit")

read_spec("spec")
DF <- dbGetQuery(con,"select * from edit")
vedvar <- dbGetQuery(con,"select * from variables")$name
va <- as.integer(CheckDF(DF,con))
vb <- as.integer(.Call("r_check_a_table",DF)$Vector)
vc <- as.integer(c(0,1,1,0,1,1))
u <- CheckConsistency(DF[2,vedvar],vedvar,"passfail",con)
v <- CheckConsistency(DF[2,vedvar],vedvar,"find_alternatives",con)
v <- sapply(1:nrow(DF),function(kr)
 return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",con))

if(!identical(va,vc) | !identical(vb,vc)) stop("Error in consistency checking")
