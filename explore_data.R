library(NLP)

dataDir <- "./tidy_data/"
dataFiles <- paste(dataDir, c("en_US.blogs.lines.txt", "en_US.news.lines.txt", "en_US.twitter.lines.txt"), 
                   sep="")

tidyData <- lapply(dataFiles, read.table, header=TRUE, sep=",")
