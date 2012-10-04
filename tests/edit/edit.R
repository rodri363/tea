library(tea) #load up TEA
set.seed(5318008) #fix the random number generator
readSpec("spec") #read in the spec; this primes the consistency checking system
doInput() #input the data as described in the input{} section of the spec

#read the data into an R data frame
DF <- dbGetQuery(teaenv$con,"select * from data")
#grab the edit variables from the database
vedvar <- dbGetQuery(teaenv$con,"select * from variables")$name
#two different ways to see if each row of the data passes all edits
va <- as.integer(CheckDF(DF,teaenv$con))
vb <- as.integer(.Call("r_check_a_table",DF)$Vector)
#a list containing the viable edit alternatives for each data row
#NULL means the row passed all edits
Le <- sapply(1:nrow(DF),function(kr)
	return(CheckConsistency(DF[kr,vedvar],vedvar,"find_alternatives",teaenv$con)))
print(Le)
