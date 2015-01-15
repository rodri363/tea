library(tea)
tryCatch({
 tc <- test_package("tea")
 }, interrupt = function(ex) {
    cat("An Interrupt was detected run-all test_package(\"tea\")\n");
    print(ex);
    quit();
  }, error = function(ex) {
    cat("An Error was detected run-all test_package(\"tea\")\n");
    print(ex);
    quit();
  }, warning = function(ex) {
    cat("A Warning was detected run-all test_package(\"tea\")\n");
    print(ex);
    quit();
})
