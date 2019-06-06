library(data.table)
source("../src/katz_backoff.R")

test <- fread("../test/tests.txt", sep=",")
index <- 1:NROW(test)

# assume all tests fail
test$success <- FALSE

# initialize time for each test
test$time <- 0

run_tests <- function(num.out=1) {
  for (II in index) {
    # run the prediction algorithm
    start.time <- Sys.time()
    res <- run_backoff(test[II]$input, num.out)
    end.time <- Sys.time()
    
    # mark success
    if (test[II,]$result %in% res$prediction) { test[II,]$success <- TRUE }
    
    # record time
    test[II,]$time <- end.time - start.time
  }
  data.table(num.tests=NROW(test), predictions=num.out, accuracy=sum(test$success)/NROW(test), 
                     avg.time=mean(test$time))
}