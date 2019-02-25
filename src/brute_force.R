brute_force <- function(graph, colors) {
  n <- length(V(graph))
  k < length(colors)
  
  colorings <- mclapply(seq(1,k**n), mc.cores = detectCores(), function(j) {
    coloring <- sapply(as.list(rep(j, n)), function(x){
      i <- parent.frame()$i[]
      index <- 1 + (x / k ** (i-1)) %% k
      return(c[index])
    })
    
    graph <- g
    V(graph)$color <- coloring
    
    if(is_colored_properly(graph)) {
      sum <- color_sum(graph, c)
      return(list(sum, coloring))
    }
    
    return(list(0, coloring))
  })
  
  g2 <- g
  V(g2)$color <- rep(4, n)
  g2sum <- color_sum(g2, c)
  
  for(i in 1:k**n) {
    tmpSum <- colorings[[i]][[1]]
    if(tmpSum != 0 && tmpSum < g2sum) {
      g2sum <- tmpSum
      V(g2)$color = colorings[[i]][[2]]
    }
  }
  
  return(g2)
}