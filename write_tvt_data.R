setwd("C:/Users/vince/Desktop/natural_language_processor")

library(caret)
library(tidytext)
library(plyr)
library(dplyr)
library(ggpubr)
library(tidyr)
set.seed(20190521)

sources <- c("blogs","news","twitter")
dir <- "./tidy_data/"
tvt.files <- paste(dir,"en_US.",sources,".tvt.txt",sep="")
output.dirs <- c("./train/","./validate/","./test/")

data <- lapply(tvt.files, read.table, header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="") %>% lapply(as_tibble)

is_word <- function(x) { grepl("^[a-z\']+$", x) }
vowel_repeats <- function(x) { grepl("(a|i|u){2,}|(e|o){3,}", x) }

create_ngrams <- function(n) {
  ngrams <- lapply(data, unnest_tokens, ngram, sentence, token="ngrams", n=2)
  ngrams <- lapply(ngrams, function(x) x[!is.na(x$ngram), ])
  numWords <- 1:n
  ngramSep <- bind_rows(ngrams) %>% separate("ngram", paste("word", numWords, sep=""), sep=" ")
  ngramSep %>% filter(is_word(word1) & is_word(word2)) %>% 
    filter(!(vowel_repeats(word1) | vowel_repeats(word2))) %>% filter(!(word1==word2))
}

write_ngrams <- function(ngrams, to.append) {
  in.train <- createDataPartition(1:NROW(ngrams), p=0.6, list=FALSE)
  ngrams.train <- ngrams[in.train,]
  write.table(ngrams.train, "./model_data/train.txt", sep=" ", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
  rm(ngrams.train)
  
  ngrams.not.train <- ngrams[-in.train,]
  in.validate <- createDataPartition(1:NROW(ngrams.not.train), p=0.5, list=FALSE)
  ngrams.validate <- ngrams.not.train[in.validate,]
  write.table(ngrams.validate, "./model_data/validate.txt", sep=" ", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
  rm(ngrams.validate)
  
  ngrams.test <- ngrams.not.train[-in.validate,]
  write.table(ngrams.test, "./model_data/test.txt", sep=" ", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
}

bigrams <- create_ngrams(2)
write_ngrams(bigrams, FALSE)
rm(bigrams)

trigrams <- create_ngrams(3)
write_ngrams(trigrams, TRUE)
rm(trigrams)

quadrigrams <- create_ngrams(4)
write_ngrams(quadrigrams, TRUE)
rm(quadrigrams)
