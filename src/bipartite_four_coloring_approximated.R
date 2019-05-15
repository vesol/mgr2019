bipartite_four_coloring_approximated <- function(graph, colors, debug = FALSE) {
  S1 <- graph
  S1v <- bipartite_two_coloring(S1, debug)
  
  V(S1)[S1v$v1]$color <- colors[1]
  V(S1)[S1v$v2]$color <- colors[2]
  sum1 <- sum_coloring(S1, colors)
  
  if (debug) {
    title <- paste('approximate1:', sum1)
    dGraph <- S1
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(as_ids(V(dGraph)), '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
  
  threePseudoColoring <- bipartite_three_pseudocoloring(graph, colors, debug)
  
  V1 <- threePseudoColoring$v1
  V2 <- threePseudoColoring$v2
  V3 <- threePseudoColoring$v3
  
  S2 <- graph
  S2small <- induced_subgraph(S2, c(V2, V3))
  if (length(V(S2small)) == 0) {
    return(list(S1, sum1))
  }
  
  S2v <- bipartite_two_coloring(S2small, debug)
  
  V(S2)[V1]$color <- colors[1]
  V(S2)[S2v$v1]$color <- colors[2]
  V(S2)[S2v$v2]$color <- colors[3]
  sum2 <- sum_coloring(S2, colors)
  
  if (debug) {
    title <- paste('approximate2:', sum2)
    dGraph <- S2
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(as_ids(V(dGraph)), '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
    
  if (length(V3) == 0) {
    if (sum1 == min(sum1, sum2)) {
      return(list(S1, sum1))
    }

    return(list(S2, sum2))
  }
  
  S3 <- graph
  S3small <- induced_subgraph(S3, V3)
  
  S3v <- bipartite_two_coloring(S3small, debug)
  
  V(S3)[V1]$color <- colors[1]
  V(S3)[V2]$color <- colors[2]
  V(S3)[S3v$v1]$color <- colors[3]
  V(S3)[S3v$v2]$color <- colors[4]
  sum3 <- sum_coloring(S3, colors)
  
  if (debug) {
    title <- paste('approximate3:', sum3)
    dGraph <- S3
    plot(dGraph, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main=title, vertex.label = paste0(as_ids(V(dGraph)), '/', V(dGraph)$weight, '/', V(dGraph)$color))
  }
  
  if (sum1 == min(sum1, sum2, sum3)) {
    return(list(S1, sum1))
  }
  
  if (sum2 == min(sum1, sum2, sum3)) {
    return(list(S2, sum2))
  }
  
  return(list(S3, sum3))
}