library(tea)
tryCatch({
  tc <- readSpec("paste_error.spec")
  }, interrupt = function(ex) {
    cat("An Interrupt was detected\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected\n");
    print(ex);
    quit();
})

doInput()
tryCatch({
  ty <- doMImpute()
  }, interrupt = function(ex) {
    cat("An Interrupt was detected\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected\n");
    print(ex);
    quit();
})
