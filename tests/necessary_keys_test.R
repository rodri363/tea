library(tea)

f<- file("spec1")
writeLines(c("input { input file: indata ",
            "overwrite: yes ",
            "output table: data} ",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f } ",
            "#checks { age > 50 and sex =\'f\' ",
            "#age <50 and sex=\'m\'} ",
            "checks { age > 50 and sex =1 ",
            "age <50 and sex=1} ",
            "impute{method: normal ",
            "output vars:age} ",
            "impute{ input table: data ",
            "method: hot deck " ,
            "output vars: sex ",
            "}"
            ), f)
close(f)

# Counter that will keep track of number of warnings that were passed
# that are related specifically to missing necessary keys 
# (for first spec file in this case)
counter1 <- 0

withCallingHandlers({
# Execute function:
print("Entering spec1 tests (1)")
browser()
readSpec("spec1")
print("Exiting spec1 tests (1)")
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    if(war$message == "You need to specify an input table in your impute key.") {
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "The first item in the config file (.spec) needs to be \"database:db_file_name.db\".") {
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
        
        
    } else if(war$message == "TEA was unable to read your spec file. This is most likely due to the fact that you didn't specify a database at the header of the file.") {
        counter1 <<- counter1 + 1
        invokeRestart("muffleWarning")
    }

# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err)) })

stopifnot(counter1 == 0)


f<- file("spec2")
writeLines(c("database: test2.db ",
            "input { input file: indata ",
            "overwrite: yes ",
            "output table: data} ",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f } ",
            "#checks { age > 50 and sex =\'f\' ",
            "#age <50 and sex=\'m\'} ",
            "checks { age > 50 and sex =1 ",
            "age <50 and sex=1} ",
            "impute{ input table: data ",
            "method: normal ",
            "output vars:age} ",
            "impute{ input table: data ",
            "method: hot deck ",
            "output vars: sex ",
            "}"
            ), f)
close(f)

unlink("test2.db")

counter2 <- 0

withCallingHandlers({
# Execute function:
print("Entering spec2 tests (2)")
readSpec("spec2")
print("Exiting spec2 tests (2)")

# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    if(war$message == "You need to specify an input table in your impute key.") {
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")


    } else if(war$message == "You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "The first item in the config file (.spec) needs to be \"database:db_file_name.db\".") {
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "TEA was unable to read your spec file. This is most likely due to the fact that you didn't specify a database at the header of the file.") {
        counter2 <<- counter2 + 1
        invokeRestart("muffleWarning")
    }


# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err)) })
stopifnot(counter2 == 0)



f<- file("spec3")
writeLines(c("database: test3.db ",
            "input { input file: indata ",
            "overwrite: yes }",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f } ",
            "checks { age > 50 and sex =1 ",
            "age <50 and sex=1} ",
            "impute{ input table: indata ",
            "output vars:age} ",
            "impute{ input table: indata ",
            "method: hot deck ",
            "output table: something ",
            "}"
            ), f)
close(f)

unlink("test3.db")

counter3 <- 0

withCallingHandlers({
# Execute function:
print("Entering spec3 tests (3)")
readSpec("spec3")
doMImpute()
print("Exiting spec3 tests (3)")
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    if(war$message == "You need to specify an input table in your impute key.") {
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "The first item in the config file (.spec) needs to be \"database:db_file_name.db\".") {
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I don't know where to write your  recodes. Please specify an output table in your spec file."){
        counter3 <<- counter3 + 1
        invokeRestart("muffleWarning")


    } else if(war$message == "TEA was unable to read your spec file. This is most likely due to the fact that you didn't specify a database at the header of the file.") {
        counter3 <<- counter3+ 1
        invokeRestart("muffleWarning")
    }


# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err)) })


stopifnot(counter3 == 3)



f<- file("spec4")
writeLines(c("database: test4.db ",
            "input { input file: indata ",
            "overwrite: yes} ",
            "fields { ",
            "age: int 1-100 ",
            "sex: cat m, f } ",
            "checks { age > 50 and sex =1 ",
            "age <50 and sex=1} ",
            "impute{ input table: data ",
            "method: normal ",
            "output vars:age} ",
            "impute{ input table: data ",
            "method: hot deck ",
            "output vars: sex ",
            "output table: something ",
            "}"
            ), f)
close(f)

unlink("test4.db")

counter4 <- 0

withCallingHandlers({
# Execute function:
print("Entering spec4 tests (4)")
readSpec("spec4")
print("Exiting spec4 tests (4)")
# Make a counter for when expected "need xxx key" occurs and 
# assert about number of such warnings after.
    }, warning=function(war) {
    if(war$message == "You need to specify an input table in your impute key.") {
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")

    } else if(war$message == "You need to specify your output vars (the variables that you would like to impute). Recall that output vars is a subkey of impute.") {
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You need to specify the method by which you would like to impute your variables. Recall that method is a subkey of the impute key.") {
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I'm going to use `filled' as a default. If you want another name than specify one in your spec file."){
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "The first item in the config file (.spec) needs to be \"database:db_file_name.db\".") {
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")
        

    } else if(war$message == "You didn't specify an output table in your input key so I don't know where to write your recodes. Please specify an output table in your spec file."){
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")


    } else if(war$message == "TEA was unable to read your spec file. This is most likely due to the fact that you didn't specify a database at the header of the file.") {
        counter4 <<- counter4 + 1
        invokeRestart("muffleWarning")
    }


# For any errors, leave the scope of readSpec(...) and
# execute the following error function:
    }, error=function(err) {
    print(paste(err)) })

stopifnot(counter4 == 1)
