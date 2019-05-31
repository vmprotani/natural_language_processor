library(readtext)
library(quanteda)
library(data.table)
library(dplyr)

data.dir <- "../Coursera-SwiftKey/final/en_US/"
sources <- c("blogs", "news", "twitter")
data.dir <- "../"
sources <- c("how_git_works", "introduction")
files <- paste(data.dir, sources, ".txt", sep="")
index <- 1:length(files)

tidy.dir <- "../tidy_data/"

data <- lapply(files, readtext)

# get profane words to filter from input
profanity <- readLines("../profanity_data/en.txt", encoding="UTF-8", skipNul=TRUE)
profane <- paste(profanity, collapse="|")

create_ngrams <- function(i, n) {
	dfm <- dfm(data[[i]]$text, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, ngram=n, remove=profane, valuetype="regex")
	df <- convert(dfm, to="data.frame")
	dt <- data.table(ngram=colnames(df)[-1], count=unlist(transpose(df[,-1])))

	dt$source <- sources[i]
	# extract last word from ngram
	if (n > 1) {
		dt$end <- unlist(lapply(strsplit(dt$ngram, "_"), tail, 1))
		dt$ngram <- gsub("_[^_]+$", "", dt$ngram)
		dt <- dt[,c("source", "ngram", "end", "count")]
	} else {
		dt <- dt[,c("source", "ngram", "count")]
	}
	names(dt)[2] <- "start"

	return(dt)
}

# create ngrams for 1-6 words
n <- 1:6
for (II in n) {
	ngrams <- lapply(index, create_ngrams, n=II)
	
	lapply(index, function(x) write.table(ngrams[[x]], paste(tidy.dir, "en_US.", sources[x], ".", II, "grams.txt", sep=""), sep=" ", row.names=FALSE, quote=FALSE))

	ngrams <- bind_rows(ngrams)
	if (II > 1) {
		ngrams <- count(ngrams[,-"source"], start, end)
	} else {
		ngrams <- count(ngrams[,-"source"], start)
	}
	cols <- names(ngrams)
	cols[length(cols)] <- "count"

	output.file <- paste(tidy.dir, "en_US.",II, "grams.txt", sep="")
	write.table(ngrams, output.file, sep=" ", row.names=FALSE, quote=FALSE)
}
