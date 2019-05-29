library(dplyr)
library(tidytext)

set.seed(20190517)

data.dir   <- "../Coursera-SwiftKey/final/en_US/"
data.sources <- c("blogs", "news", "twitter")
files     <- paste("en_US.", data.sources, ".txt", sep="")
data.files <- paste(data.dir, files, sep="")
index  <- 1:length(data.files)

tidy.dir <- "../tidy_data/"
source.files <- paste(tidy.dir, "en_US.", data.sources, ".source.txt", sep="")
tvt.files <- paste(tidy.dir, "en_US", data.sources, ".tvt.txt", sep="")

# read and subset lines in text for source and training/validation/testing sets
lines <- lapply(data.files, readLines, encoding="UTF-8", skipNul=TRUE)
random.select <- lapply(lines, function(x) matrix(data=rbinom(2*length(x), 1, 0.25), nrow=2))
source.samples <- lapply(index, function(x) tibble(text=lines[[x]][random.select[[x]][1,]==1]))
tvt.samples <- lapply(index, function(x) tibble(text=lines[[x]][random.select[[x]][2,]==1]))
rm(random.select, lines, cons, data.files, files, data.dir)

# load profanity data for filtering
profanity <- readLines("../profanity_data/en.txt", encoding="UTF-8", skipNul=TRUE)
profanity.str <- paste(profanity, collapse="|")

# tidy a list of data sets
clean_text <- function(x) {
  sentences <- lapply(x, unnest_tokens, output=sentence, input=text, token="sentences")
  
  # remove punctuation and common symbols
  sentences <- lapply(sentences, 
                      function(x) tibble(sentence=gsub("(,|\\.|!|?|\\|/|~|\'|-)+", "", x$sentence)))
  
  # remove any lines with uncommon symbols
  sentences <- lapply(sentences, function(x) x[!grepl("[^[:alnum:] ]", x$sentence), ])
  
  # remove lines with only less than 4 words
  sentences <- lapply(sentences, function(x) x[grepl("([A-Za-z]+ ){3,}", x$sentence), ])
  
  # remove lines with profanity (credits listed in ./profanity_data/README.md)
  sentences <- lapply(sentences, function(x) x[!grepl(profanity.str, x$sentence),])
  
  # remove lines with one-letter words that are not "a" or "I"
  lapply(sentences, function(x) x[!grepl("(^[^ai] )|( [^ai] )|( [^ai]$)", x$sentence),])
}
output.source <- clean_text(source.samples)
output.tvt <- clean_text(tvt.samples)

# write data to files
lapply(index, function(x) write.table(output.source[[x]], file=source.files[x], sep="\t", 
                                      row.names=FALSE, quote=FALSE))
lapply(index, function(x) write.table(output.tvt[[x]], file=tvt.files[x], sep="\t", 
                                      row.names=FALSE, quote=FALSE))
