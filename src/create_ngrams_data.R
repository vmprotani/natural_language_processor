library(tidytext)
library(dplyr)

tidy.dir <- "../tidy_data/"
sources <- c("blogs", "news", "twitter")
num.sources <- length(sources)
source.files <- paste(tidy.dir, "en_US.", sources, ".source.txt", sep="")
tvt.files <- paste(tidy.dir, "en_US.", sources, ".tvt.txt", sep="")
source("mod_ngrams.R")

source.data <- lapply(source.files, read.table, header=TRUE, sep="\t", fill=TRUE, quote="", stringsAsFactors=FALSE)
source.data <- lapply(1:num.sources, function(x) mutate(source.data[[x]], source=sources[x])) %>% lapply(as_tibble)

grams.1 <- create_ngrams(source.data, 1)
grams.1 <- clean_1grams(grams.1, TRUE)
# plot?
grams.1 <- count_all_1grams(grams.1)
write_ngrams(grams.1, "1grams")
rm(grams.1)

grams.2 <- create_ngrams(source.data, 2)
grams.2 <- clean_2grams(grams.2, TRUE)
# plot?
grams.2 <- count_all_2grams(grams.2)
write_ngrams(grams.2, "2grams")
rm(grams.2)

grams.3 <- create_ngrams(source.data, 3)
grams.3 <- clean_3grams(grams.3, TRUE)
# plot?
grams.3 <- count_all_3grams(grams.3)
write_ngrams(grams.3, "3grams")
rm(grams.3)

grams.4 <- create_ngrams(source.data, 4)
grams.4 <- clean_4grams(grams.4, TRUE)
# plot?
grams.4 <- count_all_4grams(grams.4)
write_ngrams(grams.4, "4grams")
rm(grams.4)

grams.5 <- create_ngrams(source.data, 5)
grams.5 <- clean_5grams(grams.5, TRUE)
# plot?
grams.5 <- count_all_5grams(grams.5)
write_ngrams(grams.5, "5grams")
rm(grams.5)

grams.6 <- create_ngrams(source.data, 6)
grams.6 <- clean_6grams(grams.6, TRUE)
# plot?
grams.6 <- count_all_6grams(grams.6)
write_ngrams(grams.6, "6grams")
rm(grams.6)
