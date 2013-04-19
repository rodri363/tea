library(tea)

f<- file("spec1")
writeLines(c("database:test1.db",
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
            "method: normal",
            "output vars:age}" ,
            "impute{ input table: data",
            "method: hot deck" ,
            "output vars: sex",
            "}"
            ), f)
close(f)

db <- file("test1.db")
unlink(db)

# Counter that will keep track of number of warnings that were passed
# that are related specifically to missing necessary keys 
# (for first spec file in this case)
counter1 <- 0

tryCatch({
# Execute function:
print("Entering spec1 tests")
readSpec("spec1")
doMImpute()
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    print(toString(war))

    if(toString(war) == "simpleWarning in doMImpute(): You need to specify an input table in your impute key.\n") {
        counter1 <- counter1 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter1 <- counter1 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter1 <- counter1 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter1 <- counter1 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify a database in your spec file. You must specify a database.") {
        counter1 <- counter1 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter1 <- counter1 + 1
    }

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err))

# After readSpec(...) is complete, execute the
# following finally { ... } block:
    }, finally = {
    print("Exiting readSpec() (1)")
    })
#stopifnot(counter1 == 0)



f<- file("spec2")
writeLines(c("input { input file: indata",
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
            "method: normal",
            "output vars:age}" ,
            "impute{ input table: data",
            "method: hot deck" ,
            "output vars: sex",
            "}"
            ), f)
close(f)
counter2 <- 0

tryCatch({
# Execute function:
print("Entering spec2 tests")
readSpec("spec2")

# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    print(paste(war))

    if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You need to specify an input table in your impute key.") {
        counter2 <- counter2 + 1

    } else if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter2 <- counter2 + 1

    } else if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter2 <- counter2 + 1

    } else if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter2 <- counter2 + 1

    } else if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You didn't specify a database in your spec file. You must specify a database.") {
        counter2 <- counter2 + 1

    } else if(toString(war) == "simpleWarning in readSpec(\\\"spec2\\\"): You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter2 <- counter2 + 1
    }

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err))

# After readSpec(...) is complete, execute the
# following finally { ... } block:
    }, finally = {
    print("Exiting readSpec() (2)")
    })
#stopifnot(counter2 == 1)



f<- file("spec3")
writeLines(c("database: test3.db",
            "input { input file: indata",
            "overwrite: yes",
            "output table: data}",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f }",
            "checks { age > 50 and sex =1",
            "age <50 and sex=1}",
            "impute{ input table: data",
            "output vars:age}" ,
            "impute{ input table: data",
            "method: hot deck" ,
            "output table: something",
            "}"
            ), f)
close(f)

db <- file("test3.db")
unlink(db)

counter3 <- 0

tryCatch({
# Execute function:
print("Entering spec3 tests")
readSpec("spec3")
doMImpute()
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    print(paste(war))
    
    if(toString(war) == "simpleWarning in doMImpute(): You need to specify an input table in your impute key.") {
        counter3 <- counter3 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter3 <- counter3 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter3 <- counter3 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter3 <- counter3 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify a database in your spec file. You must specify a database.") {
        counter3 <- counter3 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter3 <- counter3 + 1
    }

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err))

# After readSpec(...) is complete, execute the
# following finally { ... } block:
    }, finally = {
    print("Exiting readSpec() (3)")
    })


#stopifnot(counter3 == 3)



f<- file("spec4")
writeLines(c("database: test4.db",
            "input { input file: indata",
            "overwrite: yes}",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f }",
            "checks { age > 50 and sex =1",
            "age <50 and sex=1}",
            "impute{ input table: data",
            "method: normal",
            "output vars:age}" ,
            "impute{ input table: data",
            "method: hot deck" ,
            "output vars: sex",
            "output table: something",
            "}"
            ), f)
close(f)

db <- file("test4.db")
unlink(db)

counter4 <- 0

tryCatch({
# Execute function:
print("Entering spec4 tests")
readSpec("spec4")
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    print(paste(war))
    
    if(toString(war) == "simpleWarning in doMImpute(): You need to specify an input table in your impute key.") {
        counter4 <- counter4 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter4 <- counter4 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter4 <- counter4 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter4 <- counter4 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify a database in your spec file. You must specify a database.") {
        counter4 <- counter4 + 1

    } else if(toString(war) == "simpleWarning in doMImpute(): You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter4 <- counter4 + 1
    }

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err))

# After readSpec(...) is complete, execute the
# following finally { ... } block:
    }, finally = {
    print("Exiting readSpec() (4)")
    })


#stopifnot(counter4 == 1)
