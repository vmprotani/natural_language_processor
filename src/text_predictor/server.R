#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

# number of ngrams used in algorithm
num.ngrams <- 1:4

# location of ngram files
ngram.files <- paste0("ngrams/en_US.", num.ngrams, "grams.txt")

# max words from user's input to use as ngram
max.words  <- num.ngrams[length(num.ngrams)]-1

# ngram data
data <- lapply(ngram.files, fread, sep=",")

# how many words to predict
num.out <- 20

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$predictions <- renderDataTable({
        r <- run_backoff(input$text, to.predict=num.out)
        
        output$top <- renderText({
            r$prediction[1]
        })
        
        output$top.str <- renderText({
            paste("Top prediction (found in ngrams of", r$n[1], " words)")
        })
        
        output$all.str <- renderText({
            paste("Top", num.out, "predictions")
        })
        
        r
    }, options=NULL)
    
    output$sample.input <- renderText({ "have a great" })
    output$sample.output <- renderText({ "day\t0.287988924\t4" })
    
})

################################################################################
# runs the Katz Backoff algorithm
#
# args
#   input Uncleaned user input
#   to.prediction Number of words to output as predictions
#
# returns
#   Finalized data table of predictions and their probabilities
################################################################################
run_backoff <- function(input, to.predict=3, backoff.prob=0.4) {
    cleaned <- clean_input(input)
    ngram <- cleaned[[1]]
    num.words <- cleaned[[2]]
    if (num.words == 0) { return(NA) }
    
    # remove bulk of unigrams that won't be used
    data[[1]] <- data[[1]][1:to.predict,]
    
    predictions <- find_in_ngrams(ngram, num.words+1, to.predict, backoff.prob=backoff.prob)
    return(predictions)
}

################################################################################
# searches ngrams for matches to user input to find a word to predict
#
# args
#   input User input formatted to match beginning of ngrams
#   n Number of words in current set of ngrams (e.g., 3 for trigrams)
#   num.predictions Number of words to return as predictions
#   prev.predictions Words already chosen to predict
#
# returns
#   Data table of predictions and probabilities from current ngrams
################################################################################
find_in_ngrams <- function(input, n, num.predictions, prev.predictions=NA, backoff.prob) {
    predictions <- data.table(prediction=NA, probability=NA)
    if (n > 1) {
        # find ngrams matching the input
        ngrams <- get_found(input, n)
        num.ends <- NROW(ngrams)
        
        # store input with first word removed in case of backoff and find matches
        backoff.input <- gsub("^[a-z\'-]+_", "", input)
        
        # proceed with current ngrams (at least one is found)
        if (num.ends > 0) {
            # calculate the probability for each word using its count
            ngrams$probability <- ngrams$count / sum(ngrams$count)
            
            # use the highest probable words for the prediction
            setorder(ngrams, -probability, end)
            predictions <- ngrams[1:min(num.predictions, num.ends),]
            predictions <- predictions[,c("end", "probability")]
            names(predictions) <- c("prediction", "probability")
            predictions <- predictions[!(prediction %in% prev.predictions),]
            
            # label which ngrams predictions were found
            predictions$n <- n
            
            # search for more predictions in (n-1)grams if the number needed was not met
            if (NROW(predictions) != num.predictions) {
                current.predictions <- c(predictions$prediction, prev.predictions)
                more.predictions <- find_in_ngrams(backoff.input, n-1, num.predictions-NROW(predictions), current.predictions, backoff.prob)
                
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
            predictions <- find_in_ngrams(backoff.input, n-1, num.predictions, prev.predictions, backoff.prob)
            
            # use backoff probability if any (n-1)grams were found
            if (sum(is.na(predictions)) == 0) {
                predictions$probability <- backoff.prob * predictions$probability
            }
        }	
    }
    # if unigrams are reached, just return the number of most frequent needed
    else {
        predictions <- data[[n]][!(start %in% prev.predictions),]
        predictions <- data[[n]][1:num.predictions,]
        predictions$probability <- predictions$count / sum(predictions$count)
        predictions$count <- NULL
        names(predictions) <- c("prediction", "probability")
        predictions$n <- n
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
#   backoff.prob Probability factor for backing down to (n-1)grams
#
# returns
#   Scalar to multiply the backoff probability by
################################################################################
compute_alpha <- function(n, found, found.backoff, backoff.prob) {
    beta <- 1 - sum(compute_c(data[[n]], data[[n]][found,]$count)) / sum(data[[n]][found,]$count)
    
    # only consider probability of words in backoff that don't appear in the current ngrams
    backoffs <- data[[n-1]][found.backoff,]
    if ((n-1) > 1) {
        backoffs <- backoffs[!(end %in% data[[n]]$end),]
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

################################################################################
# finds ngrams whose beginning matches the user's input
#
# args
#   input User input
#   n N in ngrams to search
#
# returns
#   Data table containing ngrams with user's input in the start of the phrase
################################################################################
get_found <- function(input, n) {
    return(data[[n]][which(start==input),])
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
