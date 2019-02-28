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
    name_ <- paste(name, '*', sep = '')
    w <- Va[i]$weight
    
    D <- D + edge('s', name, weight = w)       # A_s1
    D <- D + edge(name_, name, weight = w * l) # A_11
  }
  
  for(i in 1:length(Vb)) {
    name <- Vb[i]$name
    name_ <- paste(name, '*', sep = '')
    
    w <- Vb[i]$weight
  
    D <- D + edge(name, 't', weight = w)       # A_2t
    D <- D + edge(name, name_, weight = w * l) # A_22
  }
  
  for(i in 1:length(E(g))) {
    w <- 1000000000 # Here should be Inf

    v1 <- V(g)[ends(g, E(g)[i])][type==TRUE]$name
    v1_ <- paste(v1, '*', sep='')
    v2 <- V(g)[ends(g, E(g)[i])][type==FALSE]$name
    v2_ <- paste(v2, '*', sep='')
    
    D <- D + edge(v1, v2, weight = w)  # A_12
    D <- D + edge(v2_, v1_, weight = w) # A_21
  }
  
  stCuts <- st_min_cuts(D, source = "s", target = "t")
  
  # for(i in 1:length(stCuts$cuts)) {
  #   cuts <- stCuts$cuts[[i]]
  #   
  #   if(all(ends(D, cuts)[,1] == 's') || all(ends(D, cuts)[,2] == 't'))  {
  #     next;
  #   }
    
    T <- D - stCuts$partition1s[[1]]
    S <- delete.vertices(D, V(T)$name)
    
    V1 <-union(intersect(V(S)$name, Va$name), intersect(V(T)$name, Vb$name))
    V2 <- gsub('[*]', '', union(intersect(V(S)$name, Vb_), intersect(V(T)$name, Va_)))
    V3 <- intersect(setdiff(V(D)$name, union(V1, V2)), union(Va$name, Vb$name))
    
    # print(V1)
    # print(V2)
    
    if (length(V3) > 0) {
      return (list(V1,V2,V3))
    }

    return(list(V1,V2))
  # }
}