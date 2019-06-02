library(data.table)
source("new_katz_backoff.R")

test <- fread("../model_data/test.txt")
names(test) <- c("input", "result")
test <- test[grepl(" ", input),]
num.tests <- 1:NROW(test)

# assume all tests fail
test$success <- 0

for (II in test) {
  res <- run_backoff(test$input[II])
  if (test$result[II] %in% res$prediction) { test$success[II] <- 1 }
}
disp <- data.table(no.tests=NROW(tests), no.success=sum(tests$success), 
                   percent=passed)
disp