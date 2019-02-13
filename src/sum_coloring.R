sum_coloring <- function(graph, colors) {
  colorWeights <- sapply(V(graph)$color, function(x) colors[x])
  vertexWeights <- V(graph)$weight
  
  return(sum(colorWeights * vertexWeights))
}