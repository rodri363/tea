library(tea)

set.seed(5318008)
kN <- 10
#	age between 3 and 6 and schl in ('b','c')
#	sex = '1' and schl='b'
DF <- data.frame(
	id=1:3,
	age=c(1,3,7),
	sex=as.character(c(1,1,2)),
	schl=as.character(c('a','b','c'))
)
#	id=1:kN,
#	age=sample(c(1:5,NA),kN,replace=TRUE),
#	sex=as.character(sample(c(1,2,NA),kN,replace=TRUE)),
#	schl=as.character(sample(c("a","b","c","d",NA),kN,replace=TRUE)))
con <- dbConnect(dbDriver("SQLite"),"db")
dbGetQuery(con,"drop table if exists edit")
dbGetQuery(con,"create table edit (id integer, age integer, sex text, schl text)")
dbGetQuery(con,"begin transaction")
dbGetPreparedQuery(con,"insert into edit values ($id,$age,$sex,$schl)",DF)
dbGetQuery(con,"commit")

read_spec("spec")
DF <- dbGetQuery(con,"select * from edit")
vedvar <- dbGetQuery(con,"select * from variables")$name
va <- as.integer(CheckDF(DF,con))
vb <- as.integer(.Call("r_check_a_table",DF)$Vector)
u <- CheckConsistency(DF[2,vedvar],vedvar,"passfail",con)
v <- CheckConsistency(DF[2,vedvar],vedvar,"find_alternatives",con)
v <- sapply(1:nrow(DF),function(kr)
 return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",con)))

#if(!identical(va,vc) | !identical(vb,vc)) stop("Error in consistency checking")
