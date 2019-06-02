library(readtext)
library(quanteda)
library(data.table)

# load common variables
source("config.R")

# configure file names
data.dir <- "../Coursera-SwiftKey/final/en_US/"
profanity.file <- "../profanity_data/en.txt"

files <- paste0(data.dir, "en_US.", sources, ".txt")
index <- 1:length(files)

# consider profane words and symbols to filter
profanity <- readLines(profanity.file, encoding="UTF-8", skipNul=TRUE)
profane <- paste(profanity, collapse="|")
unwanted <- paste(profane, "[^a-zA-Z\'_-]", sep="|")

# clean text files and separate them into ngrams
create_ngrams <- function(i, n) {
	dfm <- dfm(data[[i]], remove_numbers=TRUE, remove_punct=TRUE, 
	           remove_symbols=TRUE, remove_url=TRUE, ngram=n, remove=unwanted, 
	           valuetype="regex")
	df <- convert(dfm, to="data.frame")
	dt <- data.table(ngram=colnames(df)[-1], count=unlist(transpose(df[,-1])))
	dt$source <- sources[i]
	
	# extract last word from ngram and sort table
	sort.cols <- c("source", "ngram", "count")
	order.vars <- c("source", "count", "start")
	order.direction <- c(1, -1, 1)
	if (n > 1) {
		dt$end <- unlist(lapply(strsplit(dt$ngram, "_"), tail, 1))
		dt$ngram <- gsub("_[^_]+$", "", dt$ngram)
		sort.cols <- append(sort.cols, "end", 2)
		order.vars <- c(order.vars, "end")
		order.direction <- c(order.direction, 1)
	}
	dt <- dt[,sort.cols, with=FALSE]
	names(dt)[2] <- "start"
	setorderv(dt, order.vars, order.direction)

	return(dt)
}

# create ngrams for needed word groups
for (II in num.ngrams) {
  cons <- lapply(files, file, "r")
  out.files <- tidy.files[seq(II,num.exp.files,4)]
  
  # read each file sectionalized
  while (TRUE) {
    # so long as every file still has lines, reference them all
    use.index <- index
    use.out.files <- out.files
    
    # read a max of one million lines from each file
    data <- lapply(cons, readLines, n=250000, skipNul=TRUE, warn=FALSE)
    if (sum(unlist(lapply(data, length))) == 0) { break }
    names(data) <- sources
    
    # ignore any files that have finished being read
    for (s in sources) {
      if (length(data[names(data)==s][[1]]) == 0) { 
        data <- data[names(data)!=s]
        use.index <- 1:length(data)
        use.out.files <- use.out.files[!grepl(s, use.out.files)]
      }
    }
    
    # put each file's content in its own single string
    data <- lapply(data, paste, collapse="\n")
    
    # create ngrams for each of these file sections
  	ngrams <- lapply(use.index, create_ngrams, n=II)
  	lapply(use.index, function(x) fwrite(ngrams[[x]], out.files[x], sep=",", 
  	                                 quote=FALSE, row.names=FALSE, 
  	                                 verbose=FALSE, append=TRUE))
  	
  	rm(ngrams)
  }
  
  # combine all sections for each file's ngrams and rewrite the files
  ngrams <- lapply(out.files, fread, sep=",")
  group.list <- c("source", "start")
  order.vars <- c("source", "count", "start")
  order.direction <- c(1, -1, 1)
  if (II > 1) {
    group.list <- c(group.list, "end")
    order.vars <- c(order.vars, "end")
    order.direction <- c(order.direction, 1)
  }
  ngrams <- lapply(ngrams, function(d) d[,.(count=sum(count)), by=group.list])
  lapply(ngrams, setorderv, order.vars, order.direction)
  lapply(index, function(x) fwrite(ngrams[[x]], out.files[x], sep=",", 
                                   quote=FALSE, row.names=FALSE, verbose=FALSE,
                                   append=FALSE))
  
  # combine all files' ngrams into one without sources
  ngrams <- rbindlist(ngrams, use.names=TRUE, fill=TRUE)
  
  # remove bottom 95% of ngrams (except for unigrams, which return 1 element)
  if (II > 1) { ngrams <- ngrams[count>quantile(count, 0.95),] }
  
  # count ngrams across all sources and sort
  group.list <- c("start")
  order.vars <- c("count", "start")
  order.direct <- c(-1, 1)
  if (II > 1) {
    group.list <- c(group.list, "end")
    order.vars <- c(order.vars, "end")
    order.direct <- c(order.direct, 1)
  }
  ngrams <- ngrams[,-"source"][,.(count=sum(count)), by=group.list]
  setorderv(ngrams, order.vars, order.direct)
  fwrite(ngrams, ngram.files[II], sep=",", quote=FALSE, row.names=FALSE, 
         verbose=FALSE)
  
  # close and erase everything for next set of ngrams
  closeAllConnections()
  rm(ngrams, cons, group.list, order.vars)
}