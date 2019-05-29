# create ngrams and separate by word
create_ngrams <- function(n) {
  ngrams <- lapply(data, unnest_tokens, ngram, sentence, token="ngrams", n=n)
  ngrams <- lapply(ngrams, function(x) x[!is.na(x$ngram), ])

  num.words <- 1:n
  words <- paste("word", num.words, sep="")
  bind_rows(ngrams) %>% separate("ngram", words, sep=" ")
}

# declare filtering functions
is_word <- function(x) { grepl("^[a-z\']+$", x) }
vowel_repeats <- function(x) { grepl("(a|i|u){2,}|(e|o){3,}", x) }

# remove unwanted ngrams and count remaining
clean_ngrams <- function(x, n) { 
  index <- 1:n
  all_words <- with(x, is_word(word1))
  no_repeated_vowels <- with(x, !vowel_repeats(word1))
  no_repeated_words <- with(x, word1 != word2)
  for (II in index) {
    if (II > 1) {
      curr.word <- paste("word", II, sep="")
      curr.index <- names(x)==curr.word
      prev.word <- paste("word", II-1, sep="")
      prev.index <- names(x)==prev.word
      
      all_words <- all_words & is_word(x[,curr.index])
      no_repeated_vowels <- no_repeated_vowels & !vowel_repeats(x[,curr.index])
      no_repeated_words <- no_repeated_words & x[,curr.index] != x[,prev.index]
    }
  }
  group.words <- names(x)[1:n-1]
  count.word <- names(x)[n]
  x %>% filter(all_words & no_repeated_vowels & no_repeated_words) %>%
    group_by_at(vars(one_of(group.words))) %>% count(.dots=count.word) %>% arrange(!!sym(group.words), desc(n))
}

# combine first n-1 words to separate input from correct word
format_tvt <- function(ngrams, n) {
  input.words <- paste("word", 1:n-1, sep="")
  ngrams %>% mutate(input=apply(ngrams[,input.words], 1, paste, collapse=" ")) %>% 
    (function(x) x[,c("input", paste("word", n, sep=""))])
}

# write training/validation/testing ngrams to file
write_tvt <- function(ngrams, to.append) {
  in.train <- createDataPartition(1:NROW(ngrams), p=0.8, list=FALSE)
  ngrams.train <- ngrams[in.train,]
  write.table(ngrams.train, "../model_data/train.txt", sep=" ", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
  rm(ngrams.train)
  
  ngrams.test <- ngrams[-in.train,]
  write.table(ngrams.test, "../model_data/test.txt", sep=" ", 
              row.names=FALSE, col.names=FALSE, quote=FALSE, append=to.append)
}

# write reference ngrams to file
write_ngrams(ngrams, file.name) {
  write.table(ngrams, paste("../ngrams/", file.name, ".txt", sep=""), row.names=FALSE, quote=FALSE)
}