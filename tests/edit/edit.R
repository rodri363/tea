library(tea) #load up TEA
set.seed(5318008) #fix the random number generator
readSpec("spec") #read in the spec; this primes the consistency checking system
doInput() #input the data as described in the input{} section of the spec

#read the data into an R data frame
DF <- dbGetQuery(teaenv$con,"select * from data")
#grab the edit variables from the database
vedvar <- dbGetQuery(teaenv$con,"select * from variables")$name
#two different ways to see if each row of the data passes all edits
#CheckDF no longer seems to work...
#may consider rewriting it to use the r_check_table call
#va <- as.integer(CheckDF(DF))
vb <- as.integer(.Call("r_check_a_table",DF)$Vector)
lb <- GetAlternatives(DF[1,])
lb <- GetAlternatives(DF[3,])
#a list containing the viable edit alternatives for each data row
#NULL means the row passed all edits
#le <- sapply(1:nrow(DF),function(kr)
#	return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",teaenv$con)))
#print(le)
