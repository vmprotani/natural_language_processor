sort2 <- function(x) { 
  x %>% filter(isWord(word1) & isWord(word2)) %>% 
    filter(!(vowelRepeats(word1) | vowelRepeats(word2))) %>% filter(!(word1==word2)) %>% 
    group_by(word1) %>% count(word2) %>% arrange(word1, word2, desc(n))
}
filtered2 <- sort2(ngram2$count[rbinom(NROW(ngram2$count), 1, prob=0.6)==1,])
rm(ngram2)