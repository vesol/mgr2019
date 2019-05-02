all_edges <- function(input, index) {
  g <- input
  n1 <- length(V(g)[type==FALSE])
  n2 <- length(V(g)[type==TRUE])
  
  edges <- sapply(as.list(rep(index, n1*n2)), function(x){
    i <- parent.frame()$i[]

    index <- (x / 2^(i-1)) %% 2
    return(floor(index))
  })

  for(j in 1:length(edges)) {
    if(edges[j] == 1) {
      from <- 1 + j %% n1
      to <- n1 + ceiling(j / n2)

      g <- g + edge(V(g)[from], V(g)[to])
    }
  }
  
  return(g)
}