is_optimal <- function(graph, colors) {
  modified_colors <- c(1,2,2)
  
  V0 <- three_pseudocoloring(graph, modified_colors)[[1]]
  V1 <- three_pseudocoloring(graph, colors)[[1]]
  
  graph_V0 <- induced_subgraph(graph, V0)
  graph_V1 <- induced_subgraph(graph, V1)
  
  return(sum(V(graph_V0)$weight) == sum(V(graph_V1)$weight))
}