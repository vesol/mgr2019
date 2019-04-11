brute_force <- function(graph, colors, initialSum = 0, debug = FALSE) {
  n <- length(V(graph))
  k <- length(colors)
  
  colorings <- mclapply(seq(1,k**n), mc.cores = detectCores(), function(j) {
    coloring <- sapply(as.list(rep(j, n)), function(x){
      i <- parent.frame()$i[]
      index <- 1 + (x / k ** (i-1)) %% k
      return(c[index])
    })
    
    g <- graph
    V(g)$color <- coloring
    
    sum <- sum_coloring(g, c)
    if (sum > initialSum && initialSum != 0) {
      return(list(0, coloring))
    }
    
    if(is_colored_properly(g)) {
      return(list(sum, coloring))
    }
    
    return(list(0, coloring))
  })
  
  g2 <- graph
  V(g2)$color <- rep(4, n)
  g2sum <- sum_coloring(g2, c)
  
  for(i in 1:k**n) {
    tmpSum <- colorings[[i]][[1]]
    if(tmpSum != 0 && tmpSum < g2sum) {
      g2sum <- tmpSum
      V(g2)$color = colorings[[i]][[2]]
    }
  }
  
  bGraph <- g2
  bSum <- sum_coloring(bGraph, c)
  
  if (debug) {
    title <- paste('bruteForced:', bSum)
    dGraph <- bGraph
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(V(dGraph)$name, '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
  
  return(list(bGraph, bSum))
}