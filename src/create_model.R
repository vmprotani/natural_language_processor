num.folds <- 5

# read training/validation data
data <- read.table("../model_data/train.txt", sep="\t", stringsAsFactors=FALSE, quote="", header=FALSE)

# create the indices for training data
in.train <- matrix(rbinom(num.folds * NROW(data), 1, 0.75), nrow=5)

# separate training and validation data for each fold
train <- lapply(1:num.folds, function(x) data[in.train[x,],])
validation <- lapply(1:num.folds, function(x) data[-in.train[x,],])
rm(data)

for (i in 1:num.folds) {
### perform cross-validation
### select most accurate model
}

# read test data
data <- read.table("../model_data/test.txt", sep="\t", stringsAsFactors=FALSE, quote="", header=FALSE)

### run most accurate model against test data
