library(data.table)

# configure global variables
num.ngrams <- 1:3
ngram.files <- paste0("../backup/en_US.", num.ngrams, "grams.txt")
max.words  <- num.ngrams[length(num.ngrams)]-1
backoff.prob <- 0.4
to.predict <- 10
data <- lapply(ngram.files, fread, sep=",")

# remove bulk of unigrams that won't be used
data[[1]] <- data[[1]][1:to.predict,]

################################################################################
# runs the Katz Backoff algorithm
#
# args
#   input Uncleaned user input
#
# returns
#   Finalized data table of predictions and their probabilities
################################################################################
run_backoff <- function(input) {
  cleaned <- clean_input(input)
  ngram <- cleaned[[1]]
  num.words <- cleaned[[2]]
  if (num.words == 0) { return(NA) }
  
  predictions <- find_in_ngrams(ngram, num.words+1)
  return(predictions)
}

################################################################################
# searches ngrams for matches to user input to find a word to predict
#
# args
#   input User input formatted to match beginning of ngrams
#   n Number of words in current set of ngrams (e.g., 3 for trigrams)
#   num.predictions Number of words to return as predictions
#   current.predictions Words already chosen to predict
#
# returns
#   Data table of predictions and probabilities from current ngrams
################################################################################
find_in_ngrams <- function(input, n, num.predictions=to.predict, current.predictions=NA) {
  predictions <- data.table(prediction=NA, probability=NA)
  if (n > 1) {
    # find ngrams matching the input
    found <- get_found(input, n)
    ngrams <- data[[n]][found,]
    num.ends <- NROW(ngrams)
    
    # store input with first word removed in case of backoff
    backoff.input <- gsub("^[a-z\'-]+_", "", input)
    found.backoff <- get_found(backoff.input, n-1)
    
    # proceed with current ngrams (at least one is found)
    if (num.ends > 0) {
      # calculate the probability for each word using its count
      ngrams$probability <- ngrams$count / sum(ngrams$count)
      
      # use the highest probable words for the prediction
      setorder(ngrams, -probability, end)
      end.words <- 1:min(num.predictions, num.ends)
      predictions <- ngrams[end.words,]
      predictions <- predictions[,c("end", "probability")]
      names(predictions) <- c("prediction", "probability")
      predictions <- predictions[!prediction %in% current.predictions,]
      
      # search for more predictions in (n-1)grams if the number needed was not met
      if (NROW(predictions) != num.predictions) {
        more.predictions <- find_in_ngrams(backoff.input, n-1, num.predictions-NROW(predictions), predictions$prediction)
        
        # continue if predictions were found in the (n-1)grams
        if (sum(is.na(more.predictions)) == 0) {
          # scale backoff predictions with backoff probability
          more.predictions$probability <- backoff.prob * more.predictions$probability
          
          # include new predictions with current predictions
          predictions <- rbind(predictions, more.predictions)
        }
      }
    }
    # backoff if no matching ngrams are found
    else {
      predictions <- find_in_ngrams(backoff.input, n-1, num.predictions, current.predictions)
      
      # use backoff probability if any (n-1)grams were found
      if (sum(is.na(predictions)) == 0) {
        predictions$probability <- backoff.prob * predictions$probability
      }
    }	
  }
  # if unigrams are reached, just return the number of most frequent needed
  else {
    predictions <- data[[n]][!start %in% current.predictions,]
    predictions <- data[[n]][1:num.predictions,]
    predictions$probability <- predictions$count / sum(predictions$count)
    predictions <- predictions[,-count]
    names(predictions) <- c("prediction", "probability")
  }
  
  setorder(predictions, -probability, prediction)
  return(predictions)
}

################################################################################
# computes the count for final words in an ngram considering unobserved ngrams
#
# args
#   ngrams Current set of ngrams being used
#   counts Vector of frecuencies for end words with input matching the beginning
#
# returns
#   Numeric vector of new count values considering unobserved ngrams
################################################################################
compute_c <- function(ngrams, counts) {
  N.plus <- unlist(lapply(counts, function(n) NROW(ngrams[count==n+1,])))
  N <- unlist(lapply(counts, function(n) NROW(ngrams[count==n,])))
  
  c <- (counts+1)*N.plus/N
  c <- unlist(lapply(1:length(counts), function(x) if (is.na(c[x]) | c[x]==0 | c[x]==Inf) { counts[x] } else { c[x] }))
  
  return(c)
}

################################################################################
# computes alpha in the case that a backoff needs to be performed
#
# args
#   n Number of words in current set of ngrams (e.g., 3 for trigrams)
#   found Logical vector indicating which rows match user input in the ngrams
#   found.backoff Logical vector indicating which match user input in (n-1)grams
#
# returns
#   Scalar to multiply the backoff probability by
################################################################################
compute_alpha <- function(n, found, found.backoff) {
  beta <- 1 - sum(compute_c(data[[n]], data[[n]][found,]$count)) / sum(data[[n]][found,]$count)
  
  # only consider probability of words in backoff that don't appear in the current ngrams
  backoffs <- data[[n-1]][found.backoff,]
  if ((n-1) > 1) {
    backoffs <- backoffs[!end %in% data[[n]]$end,]
    if (NROW(backoffs) > 1) {
      c.backoff <- compute_c(data[[n-1]], backoffs$count)
      backoffs$probability <- c.backoff / sum(backoffs$count)
      alpha <- beta / sum(backoffs$probability)
    }
    else {
      alpha <- backoff.prob
    }
  }
  else {
    alpha <- backoff.prob
  }
  
  return(alpha)
}

get_found <- function(input, n) {
  index <- data[[n]]$start == input
  
  return(index)
}

################################################################################
# formats user input to match the (n-1)grams in the source data
#
# args
#   input User input
#
# returns
#   User input formatted to match ngram data
#   Number of words in user input
################################################################################
clean_input <- function(input) {
  separated.input <- unlist(strsplit(input, "[^A-Za-z\'-]+"))
  last <- length(separated.input)
  first <- max(1, last-(max.words-1))
  
  tidy.input <- separated.input[first:last]
  num.words <- length(tidy.input)
  tidy.input <- paste(tidy.input, collapse="_")
  
  ret <- list(tidy.input, num.words)
  return(ret)
}
