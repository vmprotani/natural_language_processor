sort2 <- function(x) { 
  set2 <- x %>% filter(isWord(word1) & isWord(word2)) %>% 
    filter(!(vowelRepeats(word1) | vowelRepeats(word2))) %>% filter(!(word1==word2)) %>% 
    group_by(word1) %>% count(word2) %>% arrange(word1, word2, desc(n))
  totals2 <- set2 %>% ungroup() %>% select(word2) %>% count(word2)
  inner_join(set2, totals2, by="word2") %>% arrange(word1, desc(n.x), desc(n.y), word2)  %>%
    ddply(.(word1), function(x) x[1:min(3, NROW(x)),])
}
filtered2 <- sort2(ngram2$count)
rm(ngram2)