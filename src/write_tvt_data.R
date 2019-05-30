setwd("C:/Users/vince/Desktop/natural_language_processor/src")

library(caret)
library(tidytext)
library(dplyr)

set.seed(20190521)

sources <- c("blogs","news","twitter")
dir <- "../tidy_data/"
tvt.files <- paste(dir,"en_US.",sources,".tvt.txt",sep="")
output.dir <- "../model_data"

# read data files
data <- lapply(tvt.files, read.table, header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="") %>% 
  lapply(as_tibble)

# declare ngram functions
source("mod_ngrams.R")

# combine first n-1 words to separate input from correct word
format_input <- function(ngrams, n) {
  input.words <- paste("word", 1:(n-1), sep="")
  ngrams <- ngrams %>% ungroup() %>% mutate(input=apply(ngrams[,input.words], 1, paste, collapse=" ")) %>% 
    (function(x) x[,c("input", paste("word", n, sep=""))])
  names(ngrams)[2] <- "last.word"
  ngrams
}

# create and write bigrams
bigrams <- create_ngrams(data, 2)
bigrams <- clean_bigrams(bigrams, FALSE)
bigrams <- bigrams %>% select(-n) %>% format_input(2)
write_tvt(bigrams, FALSE)
rm(bigrams)

# create and write trigrams
trigrams <- create_ngrams(data, 3)
trigrams <- clean_trigrams(trigrams, FALSE)
trigrams <- trigrams %>% select(-n) %>% format_input(3)
write_tvt(trigrams, TRUE)
rm(trigrams)

#create and write quadrigrams
quadrigrams <- create_ngrams(data, 4)
quadrigrams <- clean_quadrigrams(quadrigrams, FALSE)
quadrigrams <- quadrigrams %>% select(-n) %>% format_input(4)
write_tvt(quadrigrams, TRUE)
rm(quadrigrams)
