sort4 <- function(x) {
  set4 <- x %>% filter(isWord(word1) & isWord(word2) & isWord(word3) & isWord(word4)) %>% 
    filter(!(vowelRepeats(word1) | vowelRepeats(word2) | vowelRepeats(word3) | vowelRepeats(word4))) %>% 
    filter(!(word1==word2 | word2==word3 | word3==word4)) %>% group_by(word1, word2, word3) %>% 
    count(word4) %>% arrange(word1, word2, word3, desc(n))
  totals4 <- set4 %>% ungroup() %>% select(word4) %>% count(word4)
  inner_join(set4, totals4, by="word4") %>% arrange(word1, word2, word3, desc(n.x), desc(n.y), word4) %>%
    ddply(.(word1, word2, word3), function(x) x[1:min(3, NROW(x)),])
}
filtered4 <- sort4(ngram4$count)
rm(ngram4)