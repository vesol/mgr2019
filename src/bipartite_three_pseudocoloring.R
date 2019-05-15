bipartite_three_pseudocoloring <- function(graph, colors, debug = FALSE) {
  g <- graph
  l <- (colors[3] - colors[2]) / (colors[2] - colors[1])
  
  twoColoring <- bipartite_two_coloring(g, debug)
  Va <- V(g)[twoColoring$v1]
  Vb <- V(g)[twoColoring$v2]
  
  if(length(colors) == 2 || colors[1] == colors[2]) {
    if (debug) {
      V(g)[Va]$color <- colors[1]
      V(g)[Vb]$color <- colors[2]
      plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='3-pseudocoloring (2-coloring)', vertex.label = paste0(as_ids(V(g))))
    }
    
    return(list(v1 = Va, v2 = Vb, v3 = c()))
  }

  Va_ <- paste0(as_ids(Va), '*')
  Vb_ <- paste0(as_ids(Vb), '*')
  
  D <- make_empty_graph() + vertices(as_ids(Va)) + vertices(as_ids(Vb)) + vertices(Va_) + vertices(Vb_) + vertices(c('s', 't')) #V
  
  for(i in 1:length(Va)) {
    name <- as_ids(Va[i])
    name_ <- paste0(name, '*')
    w <- Va[i]$weight
    
    D <- D + edge('s', name, capacity = w, set = 'A_s1')
    D <- D + edge(name_, name, capacity = w * l, set = 'A_s2')
  }
  
  for(i in 1:length(Vb)) {
    name <- as_ids(Vb[i])
    name_ <- paste0(name, '*')
    
    w <- Vb[i]$weight
  
    D <- D + edge(name, 't', capacity = w, set = 'A_2t')
    D <- D + edge(name, name_, capacity = w * l, set = 'A_22')
  }
  
  for(i in 1:length(E(g))) {
    w <- Inf
    
    v1 <- as_ids(V(g)[ends(g, E(g)[i])][type==Va[1]$type])
    v1_ <- paste0(v1, '*')
    v2 <- as_ids(V(g)[ends(g, E(g)[i])][type==Vb[1]$type])
    v2_ <- paste0(v2, '*')
    
    D <- D + edge(v1, v2, capacity = w, set = 'A_12')
    D <- D + edge(v2_, v1_, capacity = w, set = 'A_21')
  }
  
  flow <- max_flow(D, source = V(D)['s'], target =V(D)['t'])
  
  VS = as_ids(flow$partition1)
  VT = as_ids(flow$partition2)
  
  '%!in%' <- function(x,y)!('%in%'(x,y))

  TVa_ <- intersect(VT, Va_)
  SVa <- paste0(intersect(as_ids(Va), VS), '*')
  move_to_S <- intersect(TVa_, SVa)
  VT <- VT[VT %!in% move_to_S]
  VS <- c(VS, move_to_S)

  SVb_ <- intersect(VS, Vb_)
  TVb <- paste0(intersect(as_ids(Vb), VT), '*')
  move_to_T <- c(intersect(SVb_, TVb))
  VS <- VS[VS %!in% move_to_T]
  VT <- c(VT, move_to_T)
  
  V1 <- union(intersect(VS, as_ids(Va)), intersect(VT, as_ids(Vb)))
  V2 <- strtoi(gsub('[*]', '', union(intersect(VS, Vb_), intersect(VT, Va_))))
  V3 <- intersect(setdiff(as_ids(V(D)), union(V1, V2)), union(as_ids(Va), as_ids(Vb)))

  if (debug) {
    V(g)[V1]$color <- colors[1]
    V(g)[V2]$color <- colors[2]
    V(g)[V3]$color <- colors[3]
    plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='3-pseudocoloring', vertex.label = paste0(as_ids(V(g))))
  }

  return (list(v1 = V1, v2 = V2, v3 = V3))
}