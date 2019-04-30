test0 <- function (graph) {
  threePseudoColoring <- three_pseudocoloring(graph, c(1,2,2))
  twoColoring <- two_coloring(graph)
  
  hiss <- sum(V(graph)[threePseudoColoring[[1]]]$weight)
  vas <- sum(V(graph)[twoColoring[[1]]]$weight)
  vbs <- sum(V(graph)[twoColoring[[2]]]$weight)
  
  if (hiss == vas || hiss == vbs) {
    return(TRUE)
  }
  
  return(FALSE)
}