random_weights_and_edges <- function(n1 = 4, n2 = 4, weights = c(1,2,6,24)) {
  m <- sample(3:(n1*n2), 1)
  g <- sample_bipartite(n1, n2, type="gnm", m=m)
  V(g)$name <- letters[1:(n1+n2)]
  V(g)$weight <- sample(weights, n, replace = TRUE)
  
  return(g)
}