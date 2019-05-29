library(caret)
library(tidytext)
library(plyr)
library(dplyr)
library(ggpubr)
library(tidyr)
set.seed(20190521)

# list tidy data files to read
dataNames <- c("blogs", "news", "twitter")
dataDir <- "../tidy_data/"
dataFileNames <- c("en_US.blogs.source.txt", "en_US.news.source.txt", "en_US.twitter.source.txt")
dataFiles <- paste(dataDir, dataFileNames, sep="")
filesIndex <- 1:length(dataFiles)

# read the files
data <- lapply(dataFiles, read.table, header=TRUE, sep="\t", fill=TRUE, quote="", stringsAsFactors=FALSE)
data <- lapply(data, function(x) tibble(sentence=x$sentence))

source("create_ngrams.R")
isWord <- function(x) { grepl("^[a-z\']+$", x) }
vowelRepeats <- function(x) { grepl("(a|i|u){2,}|(e|o){3,}", x) }

ngram2 <- createNgrams(2)
source("partition_ngrams_2.R")
write.table(filtered2, file="./ngrams/bigrams.txt", sep="\t", row.names=FALSE, quote=FALSE)
rm(bigrams, filtered2)

ngram3 <- createNgrams(3)
source("partition_ngrams_3.R")
write.table(filtered3, file="./ngrams/trigrams.txt", sep="\t", row.names=FALSE, quote=FALSE)
rm(trigrams, filtered3)

ngram4 <- createNgrams(4)
source("partition_ngrams_4.R")
write.table(filtered4, file="./ngrams/quadrigrams.txt", sep="\t", row.names=FALSE, quote=FALSE)
rm(quadrigrams, filtered4)