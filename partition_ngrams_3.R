sort3 <- function(x) {
  set3 <- x %>% filter(isWord(word1) & isWord(word2) & isWord(word3)) %>% 
    filter(!(vowelRepeats(word1) | vowelRepeats(word2) | vowelRepeats(word3))) %>% 
    filter(!(word1==word2 | word2==word3)) %>%  group_by(word1, word2) %>% count(word3) %>% 
    arrange(word1, word2, desc(n))
  totals3 <- set3 %>% ungroup() %>% select(word3) %>% count(word3)
  inner_join(set3, totals3, by="word3") %>% arrange(word1, word2, desc(n.x), desc(n.y), word3) %>%
    ddply(.(word1, word2), function(x) x[1:min(3, NROW(x)),])
}
filtered3 <- sort3(ngram3$count[rbinom(NROW(ngram3$count), 1, prob=0.6)==1,])
rm(ngram3)