three_pseudocoloring <- function(graph, colors) {
  g <- graph
  Va <- V(g)[type==TRUE]
  Vb <- V(g)[type==FALSE]
  Va_ <- paste(Va$name, '*', sep='')
  Vb_ <- paste(Vb$name, '*', sep='')
  
  l <- (colors[3] - colors[1]) / (colors[2] - colors[1])
  
  D <- make_empty_graph() + vertices(as_ids(Va)) + vertices(as_ids(Vb)) + vertices(Va_) + vertices(Vb_) + vertices(c('s', 't')) #V
  
  for(i in 1:length(Va)) {
    name <- Va[i]$name
    w <- Va[i]$weight
    D <- D + edge('s', name, weight = w)                            # A_s1
    D <- D + edge(paste(name, '*', sep = ''), name, weight = w * l) # A_11
  }
  
  for(i in 1:length(Vb)) {
    name <- Vb[i]$name
    w <- Vb[i]$weight
  
    D <- D + edge(name, 't', weight = w)                          # A_2t
    D <- D + edge(name, paste(name, '*', sep=''), weight = w * l) # A_22
  }
  
  for(i in 1:length(E(g))) {
    w <- max(V(g)$weight) * l + 1000000 # Here should be Inf

    v1 <- V(g)[ends(g, E(g)[i])][type==TRUE]
    v2 <- V(g)[ends(g, E(g)[i])][type==FALSE]
    
    D <- D + edge(v1$name, v2$name, weight = w)                                         # A_12
    D <- D + edge(paste(v2$name, '*', sep=''), paste(v1$name, '*', sep=''), weight = w) # A_21
  }
  
  stCuts <- st_min_cuts(D, source = "s", target = "t")
  
  T <- D - stCuts$partition1s[[1]]
  S <- delete.vertices(D, V(T)$name)
  
  print(V(T))
  print(V(S))
  
  V1 <-union(intersect(V(S)$name, Va$name), intersect(V(T)$name, Vb$name))
  V2 <- gsub('[*]', '', union(intersect(V(S)$name, Vb_), intersect(V(T)$name, Va_)))
  V3 <- intersect(setdiff(V(D)$name, union(V1, V2)), union(Va$name, Vb$name))
  
  print(V1)
  print(V2)
  print(V3)
  print(c(V1, V2))
  
  return(list(V1,V2,V3))
}