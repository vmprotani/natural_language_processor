# number of ngrams used in algorithm
num.ngrams <- 1:4

# location of ngram files
ngram.files <- paste0("../ngrams/en_US.", num.ngrams, "grams.txt")

# sources used to create the corpus
sources <- c("blogs", "news", "twitter")

# location of files for ngrams separated  by source
tidy.files <- paste0("../tidy_data/en_US.", rep(sources, each=4), ".",
                    rep(num.ngrams, 3), "grams.txt")

# number of ngram files separated by source
num.tidy.files <- length(exp.files)