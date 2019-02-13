colored_properly <- function(graph) {
  edges <- lapply(E(graph), function(x) {
    tmpGraph <- graph
    vertices <- ends(tmpGraph, x)
    colors <- V(tmpGraph)[vertices]$color
    
    return(length(unique(colors)) > 1)
  })
  return(!any(edges == FALSE))
}
```