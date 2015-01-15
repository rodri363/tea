library(tea)
readSpec("incl_error.spec")
tryCatch({
  tc <- doInput()
  }, interrupt = function(ex) {
    cat("An Interrupt was detected doInput\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected doInput\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected doInput\n");
    print(ex);
    quit();
})

doMImpute()
