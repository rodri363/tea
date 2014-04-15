library(tea)
tryCatch({
  tc <- readSpec("incl_error2.spec")
  }, interrupt = function(ex) {
    cat("An Interrupt was detected in readSpec\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected in readSpec\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected in readSpec\n");
    print(ex);
    quit();
})
readSpec("incl_error2.spec")
tryCatch({
  tc <- doInput()
  }, interrupt = function(ex) {
    cat("An Interrupt was detected in doInput\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected in doInput\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected in doInput\n");
    print(ex);
    quit();
})
doMImpute()
