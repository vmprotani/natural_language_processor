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
clean_1grams <- function(grams.1, include.source) {
  filtered <- grams.1 %>% filter(is_word(word1)) %>% filter(!vowel_repeats(word1))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1) %>% arrange(source, word1, desc(n))
  }
  else { 
    counted <- filtered %>% dplyr::count(word1) %>% arrange(word1, desc(n))
  }
  
  return(counted)
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
count_all_1grams <- function(grams.1) {
  counted <- grams.1 %>% select(-source) %>% group_by(word1) %>% summarize(total=sum(n)) %>% 
    arrange(word1, desc(total))
  
  return(counted)
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
clean_2grams <- function(grams.2, include.source) {
  filtered <- grams.2 %>% filter(is_word(word1) & is_word(word2)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1, word2) %>% arrange(source, word1, word2, desc(n))
  }
  else { 
    counted <- filtered %>% dplyr::count(word1, word2) %>% arrange(word1, word2, desc(n))
  }
  
  return(counted)
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
count_all_2grams <- function(grams.2) {
  counted <- grams.2 %>% select(-source) %>% group_by(word1, word2) %>% summarize(total=sum(n)) %>% 
    arrange(word1, word2, desc(total))
  
  return(counted)
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
clean_3grams <- function(grams.3, include.source) {
  filtered <- grams.3 %>% filter(is_word(word1) & is_word(word2) & is_word(word3)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & !vowel_repeats(word3))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1, word2, word3) %>% 
      arrange(source, word1, word2, word3, desc(n)) 
  }
  else { 
    counted <- filtered %>% dplyr::count(word1, word2, word3) %>% arrange(word1, word2, word3, desc(n))
  }
  
  return(counted)
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
count_all_3grams <- function(grams.3) {
  counted <- grams.3 %>% select(-source) %>% group_by(word1, word2, word3) %>% 
    summarize(total=sum(n)) %>% arrange(word1, word2, word3, desc(total))
  
  return(counted)
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
clean_4grams <- function(grams.4, include.source) {
  filtered <- grams.4 %>% filter(is_word(word1) & is_word(word2) & 
                                       is_word(word3) & is_word(word4)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & 
             !vowel_repeats(word3) & !vowel_repeats(word4))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1, word2, word3, word4) %>% 
      arrange(source, word1, word2, word3, word4, desc(n))
  }
  else { 
    counted <- filtered %>% dplyr::count(word1, word2, word3, word4) %>% 
      arrange(word1, word2, word3, word4, desc(n))
  }
  
  return(counted)
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
count_all_4grams <- function(grams.4) {
  grams.4 %>% select(-source) %>% group_by(word1, word2, word3, word4) %>% 
    summarize(total=sum(n)) %>% arrange(word1, word2, word3, word4, desc(total))
}

################################################################################
# filters unneeded 5grams and counts remaining
#
# args
#   5grams The unfiltered 5grams from the data
#   include.source Whether the 5grams data have their source
#
# return
#   a table of filtered and counted 5grams
################################################################################
clean_5grams <- function(grams.5, include.source) {
  filtered <- grams.5 %>% filter(is_word(word1) & is_word(word2) & 
                                   is_word(word3) & is_word(word4) & is_word(word5)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & 
             !vowel_repeats(word3) & !vowel_repeats(word4) & !vowel_repeats(word5))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1, word2, word3, word4, word5) %>% 
      arrange(source, word1, word2, word3, word4, word5, desc(n))
  }
  else { 
    counted <- filtered %>% dplyr::count(word1, word2, word3, word4, word5) %>% 
      arrange(word1, word2, word3, word4, word5, desc(n))
  }
  
  return(counted)
}

################################################################################
# counts 5grams across all sources
#
# args
#   grams.5 The filtered 5grams from the data
#
# return
#   a table of counted 5grams
################################################################################
count_all_5grams <- function(grams.5) {
  grams.5 %>% select(-source) %>% group_by(word1, word2, word3, word4, word5) %>% 
    summarize(total=sum(n)) %>% arrange(word1, word2, word3, word4, word5, desc(total))
}

################################################################################
# filters unneeded 6grams and counts remaining
#
# args
#   6grams The unfiltered 6grams from the data
#   include.source Whether the 6grams data have their source
#
# return
#   a table of filtered and counted 6grams
################################################################################
clean_6grams <- function(grams.6, include.source) {
  filtered <- grams.6 %>% filter(is_word(word1) & is_word(word2) & 
                                   is_word(word3) & is_word(word4) & is_word(word5) & is_word(word6)) %>% 
    filter(!vowel_repeats(word1) & !vowel_repeats(word2) & 
             !vowel_repeats(word3) & !vowel_repeats(word4) & 
             !vowel_repeats(word5) & !vowel_repeats(word6))
  if (include.source) { 
    counted <- filtered %>% dplyr::count(source, word1, word2, word3, word4, word5, word6) %>% 
      arrange(source, word1, word2, word3, word4, word5, word6, desc(n))
  }
  else { 
    counted <- filtered %>% dplyr::count(word1, word2, word3, word4, word5, word6) %>% 
      arrange(word1, word2, word3, word4, word5, word6, desc(n))
  }
  
  return(counted)
}

################################################################################
# counts 6grams across all sources
#
# args
#   grams.6 The filtered 6grams from the data
#
# return
#   a table of counted 6grams
################################################################################
count_all_6grams <- function(grams.6) {
  grams.6 %>% select(-source) %>% group_by(word1, word2, word3, word4, word5, word6) %>% 
    summarize(total=sum(n)) %>% arrange(word1, word2, word3, word4, word5, word6, desc(total))
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
