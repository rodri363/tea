library(tea)
tryCatch({
 tc <- readSpec("weight.spec")
 }, interrupt = function(ex) {
    cat("An Interrupt was detected in read_spec\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected in read_spec\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected in read_spec\n");
    print(ex);
    quit();
})
doInput()
doMImpute()
