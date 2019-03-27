two_coloring <- function(graph){
  x <- dfs(graph, V(graph)[order(weight)][1])
  va <- c()
  vb <- c()
  
  for(i in 1:length(x$order)) {
    name <- x$order[i]$name
    nb <- neighborhood(graph, 1, name)[[1]]$name
    nb <- nb[!nb %in% name]
    
    if (name %in% va || name %in% vb) {
      next
    }
    
    if(!any(nb %in% va)) {
      va <- c(va, name)
      vb <- unique(c(vb, nb))
      next
    }
    
    vb <- c(vb, name)
    va <- unique(c(va, nb))
  }
  
  if (sum(V(graph)[va]$weight) > sum(V(graph)[vb]$weight)) {
    return(list(vb, va))
  }
  
  return(list(va, vb))
}