library(dplyr)
library(tidytext)

set.seed(20190517)

dataDir   <- "./Coursera-SwiftKey/final/en_US/"
files     <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
dataFiles <- paste(dataDir, files, sep="")
numFiles  <- length(dataFiles)

# get reading connection to each file
cons <- lapply(dataFiles, file, "r")

# read and subset lines in text
lines <- lapply(cons, readLines, encoding="UTF-8", skipNul=TRUE)
lineSamples <- lapply(lines, function(x) x[rbinom(length(x), 1, 0.25)==1])

# separate lines into sentences
data <- lapply(lineSamples, function(x) tibble(line=1:length(x), text=x))
sentences <- lapply(data, unnest_tokens, output=sentence, input=text, token="sentences")

# remove lines with uncommon abbreviations
sentences <- lapply(sentences, 
                    function(x) x[!grepl("(approx|est|min|misc|mr|mrs|ms|no|temp|vs)\\.", x$sentence), ])

# remove punctuation
sentences <- lapply(sentences, 
                    function(x) tibble(line=x$line, sentence=gsub("(,|\\.|!|?)+", "", x$sentence)))

# remove any lines with symbols
sentences <- lapply(sentences, function(x) x[!grepl("[[:punct:]]", x$sentence), ])
sentences <- lapply(sentences, function(x) x[!grepl("".+"", x$sentence), ])

# remove lines with only one word
sentences <- lapply(sentences, function(x) x[grepl("([A-Za-z]+ ){1,}", x$sentence), ])

# write data to files
outputFiles <- paste("./tidy_data/", 
                          c("en_US.blogs.lines.txt", "en_US.news.lines.txt", "en_US.twitter.lines.txt"), 
                          sep="")
lapply(as.list(1:length(sentences)), 
       function(x) write.table(sentences[[x]], file=outputFiles[x], sep="\t", row.names=FALSE, 
                               quote=FALSE))
lapply(cons, close)
