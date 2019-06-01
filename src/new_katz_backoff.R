library(data.table)

# configure global variables
num.ngrams <- 1:4
ngram.files <- paste0("../ngrams", num.ngrams, "grams.txt")
max.words  <- num.ngrams[length(num.ngrams)]-1
data <- lapply(ngram.files, fread, ",")

run_backoff <- function(input) {
	ngram <- clean_input(input)
	num.words <- length(ngram)
	if (num.words == 0) { return(NA) }

	prediction <- find_in_ngrams(ngram, num.words+1)
	return(prediction)
}

find_in_ngrams <- function(input, n, num.predictions=3, current.predictions=NA) {
	if (n > 1) {
		# find ngrams matching the input
		found <- get_found(input, n)
		ngrams <- data[[n]][found,]
		num.ends <- NROW(ngrams)

		# store input with first word removed in case of backoff
		backoff.input <- gsub("^[a-z\'-]+_", "", backoff.input)
		found.backoff <- get_found(backoff.input, n-1)

		# proceed with current ngrams (at least one is found)
		if (num.ends > 0) {
			# calculate the count for all found words considering unobserved ngrams
			c <- compute_c(data[[n]], ngrams$count)
	
			# calculate the probability for each word using the updated count
			ngrams$probability <- c / sum(ngrams$count)

			# use the highest probable words for the prediction
			setorderv(ngrams, -probability, end)
			end.words <- 1:min(num.predictions, num.ends)
			predictions <- ngrams[end.words,]
			predictions <- predictions[,c("end", "probability")]
			names(predictions) <- c("prediction", "probability")
			if (sum(is.na(current.predictions)) == 0) { predictions <- predictions[!prediction %in% current.predictions,] }
			
			# search for more predictions in (n-1)grams if the number needed was not met
			if (NROW(predictions) != num.predictions) {
				more.predictions <- find_in_ngrams(backoff.input, n-1, needed.pred-NROW(predictions), predictions$prediction)

				# continue if predictions were found in the (n-1)grams
				if (sum(is.na(more.predictions)) == 0) {
					# calculate and use backoff probability
					alpha <- compute_alpha(n, found, found.backoff)
					more.predictions$probability <- alpha * more.predictions$probability

					# include new predictions with current predictions
					predictions <- rbind(predictions, more.predictions)
				}
			}
		}
		# backoff if no matching ngrams are found
		else {
			predictions <- find_in_ngrams(backoff.input, n-1, num.predictions, current.predictions)

			# continue if predictions were found in the (n-1)grams
			if (sum(is.na(more.predictions)) == 0) {
				# calculate and use backoff probability
				alpha <- compute_alpha(n, found, found.backoff)
				predictions$probability <- alpha * predictions$probability
			}
		}	
	}
	# if unigrams are reached, just return the number of most frequent needed
	else {
		predictions <- data[[n]][!start %in% current.predictions,]
		predictions <- data[[n]][1:num.predictions,]
		c <- compute_c(data[[n]], predictions$count)
		predictions$probability <- c / sum(predictions$count)
		predictions <- predictions[,-count]
		names(predictions) <- c("prediction", "probability")
	}

	setorderv(prediction, -probability, prediction)
	return(prediction)
}

compute_c <- function(ngrams, counts) {
	N.plus <- unlist(lapply(counts, function(n) NROW(ngrams[count==n+1,])))
	N <- unlist(lapply(counts, function(n) NROW(ngrams[count==n,])))

	c <- (counts+1)*N.plus/N
	c <- unlist(lapply(1:length(counts), function(x) if (is.na(c[x]) | c[x]==0 | c[x]==Inf) { counts[x] } else { c[x] }))

	return(c)
}

compute_alpha <- function(n, found, found.backoff) {
	beta <- 1 - sum(compute_c(data[[n]], data[[n]][found,]$count)) / sum(data[[n]][found,]$count)

	# only consider probability of words in backoff that don't appear in the current ngrams
	backoffs <- data[[n-1]][found.backoff,]
	if ((n-1) == 1) {
		backoffs <- backoffs[!start %in% data[[n]]$end,]
	}
	else {
		backoffs <- backoffs[!end %in% data[[n]]$end,]
	}
	c.backoff <- compute_c(data[[n-1]], backoffs$count)
	backoffs$probability <- c.backoff / sum(backoffs$count)
	alpha <- beta / sum(backoffs$probability)

	return(alpha)
}

get_found <- function(input, n) {
	index <- data$start == input

	return(index)
}

clean_input <- function(input) {
	separated.input <- unlist(strsplit(input, "[^A-Za-z\'-]+"))
	last <- length(separate.input)
	first <- max(1, last-(max.words-1))

	tidy.input <- separate.input[first:last]
	tidy.input <- paste(tidy.input, collapse="_")

	return(tidy.input)
}
