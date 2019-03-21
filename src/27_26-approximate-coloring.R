four_coloring <- function(graph, colors) {
  threePseudoColoring <- three_pseudocoloring(graph, colors)
  
  V1 <- threePseudoColoring[[1]]
  S1 <- graph
  S1v <- two_coloring(S1)
  S1a <- S1v[[1]]
  S1b <- S1v[[2]]
  
  V(S1)[name %in% S1a]$color <- colors[1]
  V(S1)[name %in% S1b]$color <- colors[2]
  sum1 <- sum_coloring(S1, colors)
  
  if (length(threePseudoColoring) == 2) {
    V3 <- c()
  } else {
    V3 <- threePseudoColoring[[3]]
  }
  
  V2 <- threePseudoColoring[[2]]
  S2 <- graph
  S2small <- induced_subgraph(S2, c(V2, V3))
  S2v <- two_coloring(S2small)
  S2a <- S2v[[1]]
  S2b <- S2v[[2]]
  
  V(S2)[name %in% V1]$color <- colors[1]
  V(S2)[name %in% S2a]$color <- colors[2]
  V(S2)[name %in% S2b]$color <- colors[3]
  sum2 <- sum_coloring(S2, colors)
  
  if (length(V3) == 0) {
    if (sum1 == min(sum1, sum2)) {
      return(list(S1,sum1))
    }
    
    return(list(S2,sum2))
  }
  
  S3 <- graph
  S3small <- induced_subgraph(S3, V3)
  
  S3v <- two_coloring(S3small)
  S3a <- S3v[[1]]
  S3b <- S3v[[2]]
  
  V(S3)[name %in% V1]$color <- colors[1]
  V(S3)[name %in% V2]$color <- colors[2]
  V(S3)[name %in% S3a]$color <- colors[4]
  V(S3)[name %in% S3b]$color <- colors[3]
  sum3 <- sum_coloring(S3, colors)
  
  if (sum1 == min(sum1, sum2, sum3)) {
    return(list(S1,sum1))
  }
  
  if (sum2 == min(sum1, sum2, sum3)) {
    return(list(S2,sum2))
  }
  
  return(list(S3,sum3))
}