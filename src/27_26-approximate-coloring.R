four_coloring <- function(graph, colors, debug = FALSE) {
  S1 <- graph
  S1v <- two_coloring(S1, debug)
  S1a <- S1v[[2]]
  S1b <- S1v[[1]]
  
  V(S1)[name %in% S1a]$color <- colors[1]
  V(S1)[name %in% S1b]$color <- colors[2]
  sum1 <- sum_coloring(S1, colors)
  
  if (debug) {
    title <- paste('approximate1:', sum1)
    dGraph <- S1
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(V(dGraph)$name, '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
  
  threePseudoColoring <- three_pseudocoloring(graph, colors, debug)
  
  V1 <- threePseudoColoring[[1]]
  if (length(threePseudoColoring) == 2) {
    V3 <- c()
  } else {
    V3 <- threePseudoColoring[[3]]
  }
  
  V2 <- threePseudoColoring[[2]]
  S2 <- graph
  S2small <- induced_subgraph(S2, c(V2, V3))
  S2v <- two_coloring(S2small)
  S2a <- S2v[[2]]
  S2b <- S2v[[1]]
  
  V(S2)[name %in% V1]$color <- colors[1]
  V(S2)[name %in% S2a]$color <- colors[2]
  V(S2)[name %in% S2b]$color <- colors[3]
  sum2 <- sum_coloring(S2, colors)
  
  if (debug) {
    title <- paste('approximate2:', sum2)
    dGraph <- S2
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(V(dGraph)$name, '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
    
  if (length(V3) == 0) {
    if (sum1 == min(sum1, sum2)) {
      return(list(S1, sum1))
    }

    return(list(S2, sum2))
  }
  
  S3 <- graph
  S3small <- induced_subgraph(S3, V3)
  
  S3v <- two_coloring(S3small)
  S3a <- S3v[[2]]
  S3b <- S3v[[1]]
  
  V(S3)[name %in% V1]$color <- colors[1]
  V(S3)[name %in% V2]$color <- colors[2]
  V(S3)[name %in% S3a]$color <- colors[3]
  V(S3)[name %in% S3b]$color <- colors[4]
  sum3 <- sum_coloring(S3, colors)
  
  if (debug) {
    title <- paste('approximate3:', sum3)
    dGraph <- S3
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(V(dGraph)$name, '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
  
  if (sum1 == min(sum1, sum2, sum3)) {
    return(list(S1, sum1))
  }
  
  if (sum2 == min(sum1, sum2, sum3)) {
    return(list(S2, sum2))
  }
  
  return(list(S3, sum3))
}