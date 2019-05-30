################################################################################
# creates ngrams separated by word
#
# args
#   data The text to separate into ngrams
#   n The number of ngrams to separate the text into
#
# return
#   a table of ngrams
################################################################################
create_ngrams <- function(data, n) {
  ngrams <- lapply(data, tidytext::unnest_tokens, ngram, sentence, token="ngrams", n=n)
  ngrams <- lapply(ngrams, function(x) x[!is.na(x$ngram),])
  
  num.words <- 1:n
  words <- paste("word", num.words, sep="")
  bind_rows(ngrams) %>% tidyr::separate("ngram", into=words, sep=" +")
}

################################################################################
# checks whether a string is a word
#
# args
#   x A vector of strings
#
# return
#   a logical vector denoting which strings are words
################################################################################
is_word <- function(x) { grepl("^[a-z\']+$", x) }

################################################################################
# checks whether a string is repeated vowels
#
# args
#   x A vector of strings
#
# return
#   a logical vector denoting which strings are repeated vowels
################################################################################
vowel_repeats <- function(x) { grepl("(a|i|u){2,}|(e|o){3,}", x) }

################################################################################
# filters unneeded unigrams and counts reamining
#
# args
#   unigrams The unfiltered unigrams from the data
#   include.source Whether the unigrams data have their source
#
# return
#   a table of filtered and counted unigrams
################################################################################
clean_unigrams <- function(unigrams, include.source) {
  filtered <- unigrams %>% filter(is_word(word1)) %>% filter(!vowel_repeats(word1))
  if (include.source) { filtered <- filtered %>% group_by(source, word1) }
  else { filtered <- filtered %>% group_by(word1) }
  counted <- filtered %>% dplyr::count(word1)
  if (include.source) { counted %>% arrange(source, word1, desc(n)) }
  else { counted %>% arrange(word1, desc(n)) }
}

################################################################################
# counts unigrams across all sources
#
# args
#   unigrams The filtered unigrams from the data
#
# return
#   a table of counted unigrams
################################################################################
count_all_unigrams <- function(unigrams) {
  unigrams %>% ungroup() %>% select(-source) %>% group_by(word1) %>% summarize(total=sum(n)) %>% 
    arrange(desc(total), word1)
}

################################################################################
# filters unneeded bigrams and counts remaining
#
# args
#   trigrams The unfiltered trigrams from the data
#   include.source Whether the trigrams data have their source
#
# return
#   a table of filtered and counted trigrams
################################################################################
clean_bigrams <- function(bigrams, include.source) {
  filtered <- bigrams %>% filter(is_word(word1) & is_word(word2)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2)) %>%
    filter(word1 != word2)
  if (include.source) { filtered <- filtered %>% group_by(source, word1) }
  else { filtered <- filtered %>% group_by(word1) }
  counted <- filtered %>% dplyr::count(word2)
  if (include.source) { counted %>% arrange(source, word1, desc(n)) }
  else { counted %>% arrange(word1, desc(n)) }
}

################################################################################
# counts bigrams across all sources
#
# args
#   bigrams The filtered bigrams from the data
#
# return
#   a table of counted bigrams
################################################################################
count_all_bigrams <- function(bigrams) {
  bigrams %>% ungroup() %>% select(-source) %>% group_by(word1, word2) %>% summarize(total=sum(n)) %>% 
    arrange(desc(total), word1, word2)
}

################################################################################
# filters unneeded trigrams and counts remaining
#
# args
#   bigrams The unfiltered bigrams from the data
#   include.source Whether the bigrams data have their source
#
# return
#   a table of filtered and counted bigrams
################################################################################
clean_trigrams <- function(trigrams, include.source) {
  filtered <- trigrams %>% filter(is_word(word1) & is_word(word2) & is_word(word3)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & !vowel_repeats(word3)) %>%
    filter(word1 != word2 & word2 != word3)
  if (include.source) { filtered <- filtered %>% group_by(source, word1, word2) }
  else { filtered <- filtered %>% group_by(word1, word2) }
  counted <- filtered %>% dplyr::count(word3)
  if (include.source) { counted %>% arrange(source, word1, word2, desc(n)) }
  else { counted %>% arrange(word1, word2, desc(n)) }  
}

################################################################################
# counts trigrams across all sources
#
# args
#   trigrams The filtered trigrams from the data
#
# return
#   a table of counted trigrams
################################################################################
count_all_trigrams <- function(trigrams) {
  trigrams %>% ungroup() %>% select(-source) %>% group_by(word1, word2, word3) %>% 
    summarize(total=sum(n)) %>% arrange(desc(total), word1, word2, word3)
}

################################################################################
# filters unneeded trigrams and counts remaining
#
# args
#   quadrigrams The unfiltered quadrigrams from the data
#   include.source Whether the quadrigrams data have their source
#
# return
#   a table of filtered and counted quadrigrams
################################################################################
clean_quadrigrams <- function(quadrigrams, include.source) {
  filtered <- quadrigrams %>% filter(is_word(word1) & is_word(word2) & 
                                       is_word(word3) & is_word(word4)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & 
             !vowel_repeats(word3) & !vowel_repeats(word4)) %>%
    filter(word1 != word2 & word2 != word3 & word3 != word4)
  if (include.source) { filtered <- filtered %>% group_by(source, word1, word2, word3) }
  else { filtered <- filtered %>% group_by(word1, word2, word3) }
  counted <- filtered %>% dplyr::count(word4)
  if (include.source) { counted %>% arrange(source, word1, word2, word3, desc(n)) }
  else { counted %>% arrange(word1, word2, word3, desc(n)) } 
}

################################################################################
# counts quadrigrams across all sources
#
# args
#   quadrigrams The filtered quadrigrams from the data
#
# return
#   a table of counted quadrigrams
################################################################################
count_all_quadrigrams <- function(quadrigrams) {
  quadrigrams %>% ungroup() %>% select(-source) %>% group_by(word1, word2, word3, word4) %>% 
    summarize(total=sum(n)) %>% arrange(desc(total), word1, word2, word3, word4)
}

################################################################################
# combines first n-1 words to separate input from correct word
#
# args
#   ngrams Table of ngrams to separate
#   n Number of words in ngrams
#
# return
#   a table with the ngrams separated between the first n-1 words and the last
################################################################################
clean_tvt <- function(ngrams, n) {
  input.words <- paste("word", 1:n-1, sep="")
  ngrams %>% mutate(input=apply(ngrams[,input.words], 1, paste, collapse=" ")) %>% 
    (function(x) x[,c("input", paste("word", n, sep=""))])
}

################################################################################
# writes reference ngrams to file
#
# args
#   ngrams Table of ngrams to write to file
#   file.name Name of file to write
################################################################################
write_ngrams <- function(ngrams, file.name) {
  file <- paste("../ngrams/", file.name, ".txt", sep="")
  write.table(ngrams, file, sep="\t", row.names=FALSE, quote=FALSE)
}

################################################################################
# writes training/validation/testing ngrams to file
#
# args
#   ngrams Table of ngrams to write to file
#   to.append Whether to append to an existing file or not
################################################################################
write_tvt <- function(ngrams, to.append) {
  in.train <- createDataPartition(1:NROW(ngrams), p=0.8, list=FALSE)
  ngrams.train <- ngrams[in.train,]
  write.table(ngrams.train, "../model_data/train.txt", sep="\t", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
  rm(ngrams.train)
  
  ngrams.test <- ngrams[-in.train,]
  write.table(ngrams.test, "../model_data/test.txt", sep="\t", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
}
