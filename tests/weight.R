library(tea)
tryCatch({
 tc <- readSpec("weight.spec")
  }, interrupt = function(ex) {
    cat("An Interrupt was detected readSpec\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected readSpec\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected readSpec\n");
    print(ex);
    quit();
})

doInput()
tryCatch({
  th <- doMImpute()
 }, interrupt = function(ex) {
    cat("An Interrupt was detected doMImpute\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected doMImpute\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected doMImpute\n");
    print(ex);
    quit();
})
