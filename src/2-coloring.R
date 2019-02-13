two_coloring <- function(graph){
  gv <- V(graph)[order(weight)]
  va <- c()
  vb <- c()
  
  x = dfs(graph, gv[1])
  for(i in 1:length(x$order)) {
    vertex = gv[x$order[i]]
    name <- vertex$name
    nb <- neighbors(graph, vertex)$name
    
    if (!any(nb %in% va)) {
      va <- c(va, name)
    } else {
      vb <- c(vb, name)
    }
  }
  
  return(list(va, vb))
}