createNgrams <- function(n) {
  # separate ngrams in sentences
  ngrams <- lapply(data, unnest_tokens, ngram, sentence, token="ngrams", n=n)
  ngrams <- lapply(ngrams, function(x) x[!is.na(x$ngram), ])
  ngramCounts <- lapply(ngrams, count, ngram, sort=TRUE)
  
  # get most common ngrams
  topNgrams <- lapply(ngramCounts, function(x) x[1:10, ])
  topNgrams <- lapply(filesIndex, function(x) {
    topNgrams[[x]] %>% mutate(file=dataFileNames[x], ngram=reorder(ngram, n))
  })
  
  # create plots for file-separated ngrams
  p <- lapply(filesIndex, function(x) {
    ggplot(topNgrams[[x]], aes(x=ngram, y=n, fill=n)) + geom_col() + coord_flip() +
      xlab(NULL) + ylab(NULL) + ggtitle(dataNames[[x]]) + theme(legend.position="none")
  })
  
  # combine ngrams across files
  combinedLevels <- union(levels(topNgrams[[1]]$ngram), levels(topNgrams[[2]]$ngram)) %>%
    union(levels(topNgrams[[3]]$ngram))
  topNgrams <- lapply(topNgrams, mutate, ngram=factor(ngram, levels=combinedLevels), 
                      file=NULL) %>% bind_rows()
  topNgramsTotal <- aggregate(n~ngram, topNgrams, sum) %>% mutate(ngram=reorder(ngram, n))
  
  # create figure with ngram plots
  left <- ggplot(topNgramsTotal, aes(x=ngram, y=n, fill=n)) + geom_col() + coord_flip() +
    xlab(NULL) + ylab(NULL) + ggtitle("all") + theme(legend.position="none")
  right <- ggarrange(p[[1]], p[[2]], p[[3]], ncol=1, nrow=3)
  
  # separate ngrams into columns
  numWords <- 1:n
  ngramSep <- bind_rows(ngramCounts) %>% separate("ngram", paste("word", numWords, sep=""), sep=" ")
  
  # return separated ngrams and figure
  ngram2 <- list(count=ngramSep, fig=annotate_figure(
    ggarrange(left, right, ncol=2, nrow=1) + theme(plot.margin=unit(c(0.3,0,0,0), "cm")),
    fig.lab=paste(n, "-word ngrams", sep=""), fig.lab.pos="top", fig.lab.face="bold"))
}