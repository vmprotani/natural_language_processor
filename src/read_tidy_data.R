# list tidy data files to read
dataNames <- c("blogs", "news", "twitter")
dataDir <- "../tidy_data/"
dataFileNames <- c("en_US.blogs.lines.txt", "en_US.news.lines.txt", "en_US.twitter.lines.txt")
dataFiles <- paste(dataDir, dataFileNames, sep="")
filesIndex <- 1:length(dataFiles)

# read the files
data <- lapply(dataFiles, read.table, header=TRUE, sep="\t", fill=TRUE, quote="", stringsAsFactors=FALSE)
data <- lapply(data, function(x) tibble(sentence=x$sentence))