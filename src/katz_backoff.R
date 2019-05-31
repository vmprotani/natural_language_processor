################################################################################
# begins the Katz Backoff Model algorithm
#
# args
#   input User string input
#
# returns
#   A word prediction based on the input
################################################################################
run_backoff <- function(input) {
  ngram <- clean_input(input)
  num.words <- length(ngram)
  
  if (num.words == 0 ) {
    return(NULL)
  }
  
  prediction <- find_in_ngram(ngram, num.words+1)
  
  return(prediction)
}

################################################################################
# looks for an ngram in the quadrigrams data
#
# args
#   input User string input
#   n Ngrams to start with
#   prev.found Logical vector for (n+1)grams found before backing off
#
# returns
#   The word prediction based on input
################################################################################
find_in_ngram <- function(input, n, prev.found=NULL) {
  # find matching ngrams
  found.index <- get_found(input, n)
  if (sum(is.na(found.index)) >= 1) { return(NULL) }
  found.ngrams <- data[[n]][found.index,]
  num.ends.found <- NROW(found.ngrams)
  # if calculating alpha, exclude the words that were found in the (n+1)grams
  if (sum(is.na(prev.found)) == 0) {
    unique.end.words <- found.ngrams[,n] %in% data[[n+1]][prev.found,n+1]
    found.ngrams <- with(found.ngrams, found.ngrams[!unique.end.words])
  }
  # input with first word removed in case of backoff
  backoff.input <- input[-1]
  
  # at least one matching ngram is found
  if (num.ends.found > 0) {
    end.words <- 1:num.ends.found
    
    # calculate the count considering unobserved ngrams
    c <- compute_c(data[[n]], found.ngrams[end.words,]$total)
    c <- lapply(end.words, 
           function(x) if (is.na(c[x]) | c[x] == 0 | c[x] == Inf) { found.ngrams[x,]$total } 
           else { c[x] }) %>% unlist()
    # calculate the probability using the count
    p.bo <- c / sum(found.ngrams$total)
    
    prediction <- tibble(word=found.ngrams[end.words,n], prob=p.bo)
    more_predictions <- find_in_ngram(backoff.input, n-1)
    if (!sum(is.na(more_predictions)) >=1) {
      prediction <- bind_rows(prediction, more_predictions)
    }
  }
  # no matching ngrams were found
  else {
    prediction <- find_in_ngram(backoff.input, n-1)
    
    found.backoff <- get_found(backoff.input, n-1)
    prediction$prob <- compute_alpha(data[[n]], found.index, backoff.input) * prediction$prob
  }
  
  prediction <- arrange(prediction, desc(prob)) %>% (function(x) x[1:min(3, NROW(x)),])
  return(prediction)
}

################################################################################
# computes Good-Turing discount estimate
#
# args
#   ngrams Table of ngrams to consider
#   count Number of times the found ngram has occurred
#
# returns
#   Good-Turing discount estimate, the count estimate considering unobserved
#   ngrams
################################################################################
compute_c <- function(ngrams, count) {
  N.plus <- lapply(count, function(x) NROW(with(ngrams, ngrams[total==x+1,]))) %>% unlist()
  N <- lapply(count, function(x) NROW(with(ngrams, ngrams[total==x,]))) %>% unlist()
  
  new.c <- (count+1)*N.plus/N
  
  return(new.c)
}

################################################################################
# computes backoff weight
#
# args
#   n N in ngrams to consider
#   found.index Logical vector of which rows have the found ngram from input
#   found.backoff Logical vector for the found (n-1)grams from input
#
# returns
#   Backoff weight by which to multiply probably of a prediction found by
#   backing off
################################################################################
compute_alpha <- function(n, found.index, backoff.input) {
  beta <- 1 - 
    sum(compute_c(data[[n]], data[[n]][found.index,]$total)) / sum(data[[n]][found.index,]$total)
  
  alpha.probs <- find_in_ngram(backoff.input, n-1, found.index)
  alpha <- beta/sum(alpha.probs$prob)

  return(alpha)
}

################################################################################
# gets ngrams where the first n-1 words match the input
#
# args
#   input User string input
#   n Number of words in ngrams to consider
#
# returns
#   Logical vector for which rows have matching ngrams
################################################################################
get_found <- function(input, n) {
  if (n == 4) {
    found.index <- with(data[[n]], word1==input[1] & word2==input[2] & word3==input[3])
  }
  else if (n == 3) {
    found.index <- with(data[[n]], word1==input[1] & word2==input[2])
  }
  else if (n == 2) {
    found.index <- with(data[[n]], word1==input[1])
  }
  else if (n ==1 ) {
    found.index <- rep(TRUE, NROW(data[[n]]))
  }
  else {
    found.index <- NULL
  }
  
  return(found.index)
}

################################################################################
# turns user input into an ngram of at most 3 words
#
# args
#   input User string input
#
# returns
#   The user input converted into an ngram
################################################################################
clean_input <- function(input) {
  separate.input <- unlist(strsplit(input, "[^A-Za-z\'-]+"))
  last <- length(separate.input)
  first <- max(1, last-(max.ngrams+1))
  
  tidy.input <- separate.input[first:last]
  tidy.input <- gsub("[-\']", "", tidy.input)
  
  return(tidy.input)
}

################################################################################
# reads data from the ngram source files

# returns
#   Ngram source data
################################################################################
read_data <- function() {
  ngrams <- c("1grams", "2grams", "3grams", "4grams", "5grams", "6grams")
  files <- paste("../ngrams/", ngrams, ".txt", sep="")
  
  data <- lapply(files, read.table, header=TRUE, sep="\t", fill=TRUE, quote="", stringsAsFactors=FALSE)
  data <- lapply(data, as_tibble)
  
  return(data)
}
max.ngrams <- 5
data <- read_data()