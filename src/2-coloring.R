two_coloring <- function(graph, debug = FALSE){
  g <- graph
  x <- dfs(g, V(g)[order(-weight)][1])
  va <- c()
  vb <- c()
  
  if (debug) {
    print(x$order)
  }
  
  for(i in 1:length(x$order)) {
    name <- x$order[i]$name
    nb <- neighborhood(graph, 1, name)[[1]]
    
    if (name %in% va || name %in% vb) {
      next
    }
    
    nb <- nb[!nb %in% name][order(-weight)]
    
    if(!any(nb %in% va)) {
      va <- c(va, name)
      vb <- unique(c(vb, nb))
      next
    }
    
    vb <- c(vb, name)
    va <- unique(c(va, nb))
  }
  
  if (sum(V(graph)[va]$weight) > sum(V(graph)[vb]$weight)) {
    v1 <- vb
    v2 <- va
  } else {
    v1 <- va
    v2 <- vb
  }
  
  if (debug) {
    V(g)[name %in% v1]$color <- 2
    V(g)[name %in% v2]$color <- 1
    plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='2-coloring', vertex.label = paste0(V(g)$name, '/', V(g)$weight, '/', V(g)$color))
  }
  
  return(list(v1, v2))
}