calculate_discount <- function(ngrams) {
  ngrams$discount <- rep(1, NROW(ngrams))
  
  for (II in 1:5) {
    n_curr <- NROW(ngrams[n==i,])
    n_next <- NROW(ngrams[n==i+1,])
    
    d_curr <- ((i+1)/i) * (n_next/n_curr)
    
    ngrams[n==i, ]$discount <- d_curr
  }
}

calculate_leftover <- function(count, discount) {
  1 - sum((discount * count)/sum(count))
}
