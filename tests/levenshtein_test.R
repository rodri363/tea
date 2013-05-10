library(tea)

f<- file("spec1")
writeLines(c("database: test1.db ",
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
            "impute{method: normal ",
            "output vars:age} ",
            "impute{ input table: data ",
            "method: hot deck " ,
            "output vars: sex ",
            "}"
            ), f)
close(f)

unlink("test1.db")

# Counter that will keep track of number of warnings that were passed
# that are related specifically to missing necessary keys 
# (for first spec file in this case)
counter1 <- 0

withCallingHandlers({
# Execute function:
print("Entering spec1 tests (1)")
readSpec("spec1")
print("Exiting spec1 tests (1)")
    }, warning=function(war) {

# Insert warnings checks here
        
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

# Insert warnings test here

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
# Insert warnings tests here


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

# Insert warnings tests here

# execute the following error function:
    }, error=function(err) {
    print(paste(err)) })

stopifnot(counter4 == 1)
