bipartite_two_coloring <- function(graph, debug = FALSE){
  g <- graph
  x <- dfs(g, V(g)[1])
  va <- c()
  vb <- c()
  
  for(i in 1:length(x$order)) {
    name <- as_ids(x$order[i])
    nb <- as_ids(neighborhood(g, 1, name)[[1]])
    others <- nb[nb != name]
    
    if (name %in% va) {
      vb <- unique(c(vb, others))
    } else if (name %in% vb) {
      va <- unique(c(va, others))
    } else if (any(others %in% vb)) {
      vb <- c(vb, name)
      va <- unique(c(va, others))
    } else if (any(others %in% va)) {
      va <- c(va, name)
      vb <- unique(c(vb, others))
    }
    
    if (sum(V(g)[va]$weight) < sum(V(g)[vb]$weight)) {
      tmpA <- va
      tmpB <- vb
      va <- tmpB
      vb <- tmpA
    }
    
    if (!name %in% va & !name %in% vb) {
      va <- c(va, name)
      vb <- unique(c(vb, others))
    }
  }
  
  if (debug) {
    V(g)[va]$color <- 1
    V(g)[vb]$color <- 2
    plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='2-coloring', vertex.label = paste0(as_ids(V(g))))
  }
  
  return(list(v1 = va, v2 = vb, v1sum = sum(V(g)[va]$weight), v2Sum = sum(V(g)[vb]$weight)))
}