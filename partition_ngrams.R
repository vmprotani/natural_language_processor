hasNum <- function(x) { grepl("[[:digit:]]+", x) }

# separate training and test data for 2-word ngrams
sort2 <- function(x) { 
  x %>% filter(!hasNum(word1) & !hasNum(word2)) %>% group_by(word1) %>% count(word2) %>%
    arrange(word1, word2, desc(n))
}
inTrain2 <- createDataPartition(1:NROW(ngram2$count), p=0.7, list=FALSE)
train2 <- sort2(ngram2$count[inTrain2,])
test2 <- sort2(ngram2$count[-inTrain2,])

# separate training and test data for 3-word ngrams
sort3 <- function(x) {
  x %>% filter(!hasNum(word1) & !hasNum(word2) & !hasNum(word3)) %>% 
    group_by(word1, word2) %>% count(word3) %>% arrange(word1, word2, desc(n))
}
inTrain3 <- createDataPartition(1:NROW(ngram3$count), p=0.7, list=FALSE)
train3 <- sort3(ngram3$count[inTrain3,])
test3 <- sort3(ngram3$count[-inTrain3,])

# separate training and test data for 4-word ngrams
sort4 <- function(x) {
  x %>% filter(!hasNum(word1) & !hasNum(word2) & !hasNum(word3) & !hasNum(word4)) %>% 
    group_by(word1, word2, word3) %>% count(word4) %>% arrange(word1, word2, word3, desc(n))
}
inTrain4 <- createDataPartition(1:NROW(ngram4$count), p=0.7, list=FALSE)
train4 <- sort4(ngram4$count[inTrain4,])
test4 <- sort4(ngram4$count[-inTrain4,])