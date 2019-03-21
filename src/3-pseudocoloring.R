three_pseudocoloring <- function(graph, colors) {
  g <- graph
  l <- (colors[3] - colors[1]) / (colors[2] - colors[1])
  
  if (sum(V(g)[type==FALSE]$weight) < sum(V(g)[type==TRUE]$weight)) {
    Va <- V(g)[type==FALSE]
    Vb <- V(g)[type==TRUE]
  } else {
    Va <- V(g)[type==TRUE]
    Vb <- V(g)[type==FALSE]
  }

  Va_ <- paste(Va$name, '*', sep='')
  Vb_ <- paste(Vb$name, '*', sep='')
  
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
    w <- max(V(g)$weight) ** 2  # Here should be Inf, but unfortunetaly Inf break algorithm

    # w <- Inf-100
    
    v1 <- V(g)[ends(g, E(g)[i])][type==Va[1]$type]$name
    v1_ <- paste(v1, '*', sep='')
    v2 <- V(g)[ends(g, E(g)[i])][type==Vb[1]$type]$name
    v2_ <- paste(v2, '*', sep='')
    
    D <- D + edge(v1, v2, weight = w)  # A_12
    D <- D + edge(v2_, v1_, weight = w) # A_21
  }
  
  stCuts <- st_min_cuts(D, source = "s", target = "t")
  T <- D - stCuts$partition1s[[1]]
  S <- delete.vertices(D, V(T)$name)
  VT = V(T)$name
  VS = V(S)$name
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  TVa_ <- intersect(VT, Va_)
  SVa <- paste(intersect(Va, VS), '*', sep='')
  move_to_S <- c(intersect(TVa_, SVa))
  VT <- VT[VT %!in% move_to_S]
  VS <- c(VS, move_to_S)
  
  SVb_ <- intersect(VS, Vb_)
  TVb <- paste(intersect(Vb, VT), '*', sep='')
  move_to_T <- c(intersect(SVb_, TVb))
  VS <- VS[VS %!in% move_to_T]
  VT <- c(VT, move_to_T)
  
  V1 <-union(intersect(VS, Va$name), intersect(VT, Vb$name))
  V2 <- gsub('[*]', '', union(intersect(VS, Vb_), intersect(VT, Va_)))
  V3 <- intersect(setdiff(V(D)$name, union(V1, V2)), union(Va$name, Vb$name))

  if (length(V3) > 0) {
    return (list(V1,V2,V3))
  }

  return(list(V1,V2))
}