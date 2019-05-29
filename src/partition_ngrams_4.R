sort4 <- function(x) {
  set4 <- x %>% filter(isWord(word1) & isWord(word2) & isWord(word3) & isWord(word4)) %>% 
    filter(!(vowelRepeats(word1) | vowelRepeats(word2) | vowelRepeats(word3) | vowelRepeats(word4))) %>% 
    filter(!(word1==word2 | word2==word3 | word3==word4)) %>% group_by(word1, word2, word3) %>% 
    count(word4) %>% arrange(word1, word2, word3, desc(n))
}
filtered4 <- sort4(ngram4$count[rbinom(NROW(ngram4$count), 1, prob=0.6)==1,])
rm(ngram4)