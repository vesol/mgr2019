two_coloring <- function(graph, debug = FALSE){
  g <- graph
  x <- dfs(g, V(g)[1])
  va <- c()
  vb <- c()
  
  for(i in 1:length(x$order)) {
    v <- x$order[i]
    name <- v$name
    nb <- neighborhood(graph, 1, name)[[1]]$name
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
    
    if (sum(V(graph)[va]$weight) < sum(V(graph)[vb]$weight)) {
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
    V(g)[name %in% va]$color <- 1
    V(g)[name %in% vb]$color <- 2
    plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='2-coloring', vertex.label = paste0(V(g)$name, '/', V(g)$weight, '/', V(g)$color))
  }
  
  return(list(va, vb))
}