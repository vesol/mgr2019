is_colored_properly <- function(graph) {
  verticleColoredProperly <- lapply(V(graph), function(i) {
    v <- V(graph)[i]
    n <- neighbors(graph, v)
    
    return(all(n$color != v$color))
  })
  
  return(all(verticleColoredProperly == TRUE))
}