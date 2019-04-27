three_pseudocoloring <- function(graph, colors, debug = FALSE) {
  g <- graph
  l <- (colors[3] - colors[2]) / (colors[2] - colors[1])
  
  twoColoring <- two_coloring(g)
  Va <- V(g)[name %in% twoColoring[[1]]]
  Vb <- V(g)[name %in% twoColoring[[2]]]
  
  if(length(Vb) == 0) {
    if (debug) {
      V(g)$color <- 1
      plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='3-pseudocoloring', vertex.label = paste0(V(g)$name, '/', V(g)$weight, '/', V(g)$color))
    }
    
    return(list(Va$name, c(), c()))
  }

  Va_ <- paste0(Va$name, '*')
  Vb_ <- paste0(Vb$name, '*')
  
  D <- make_empty_graph() + vertices(as_ids(Va)) + vertices(as_ids(Vb)) + vertices(Va_) + vertices(Vb_) + vertices(c('s', 't')) #V
  V(D)$color <- 'orange'
  
  for(i in 1:length(Va)) {
    name <- Va[i]$name
    name_ <- paste0(name, '*')
    w <- Va[i]$weight
    
    D <- D + edge('s', name, capacity = w, set = 'A_s1')
    D <- D + edge(name_, name, capacity = w * l, set = 'A_s2')
  }
  
  for(i in 1:length(Vb)) {
    name <- Vb[i]$name
    name_ <- paste0(name, '*')
    
    w <- Vb[i]$weight
  
    D <- D + edge(name, 't', capacity = w, set = 'A_2t')
    D <- D + edge(name, name_, capacity = w * l, set = 'A_22')
  }
  
  for(i in 1:length(E(g))) {
    w <- Inf
    
    v1 <- V(g)[ends(g, E(g)[i])][type==Va[1]$type]$name
    v1_ <- paste0(v1, '*')
    v2 <- V(g)[ends(g, E(g)[i])][type==Vb[1]$type]$name
    v2_ <- paste0(v2, '*')
    
    D <- D + edge(v1, v2, capacity = w, set = 'A_12')
    D <- D + edge(v2_, v1_, capacity = w, set = 'A_21')
  }
  
  flow <- max_flow(D, source = V(D)['s'], target =V(D)['t'])
  
  if (debug) {
    print(flow)
    # tkplot(D, edge.label = paste(E(D)$capacity, '/' , E(D)$set), canvas.height = 1200, canvas.width = 1200)
  }
  
  VS = as_ids(flow$partition1)
  VT = as_ids(flow$partition2)
   
  '%!in%' <- function(x,y)!('%in%'(x,y))

  TVa_ <- intersect(VT, Va_)
  SVa <- paste0(intersect(Va$name, VS), '*')
  move_to_S <- intersect(TVa_, SVa)
  VT <- VT[VT %!in% move_to_S]
  VS <- c(VS, move_to_S)

  SVb_ <- intersect(VS, Vb_)
  TVb <- paste0(intersect(Vb$name, VT), '*')
  move_to_T <- c(intersect(SVb_, TVb))
  VS <- VS[VS %!in% move_to_T]
  VT <- c(VT, move_to_T)

  V1 <-union(intersect(VS, Va$name), intersect(VT, Vb$name))
  V2 <- gsub('[*]', '', union(intersect(VS, Vb_), intersect(VT, Va_)))
  V3 <- intersect(setdiff(V(D)$name, union(V1, V2)), union(Va$name, Vb$name))

  if (debug) {
    V(g)[name %in% V1]$color <- 1
    V(g)[name %in% V2]$color <- 2
    V(g)[name %in% V3]$color <- 3
    plot(g, layout=layout_as_bipartite, palette=diverging_pal(4), vertex.size=40, vertex.label.cex=1, main='3-pseudocoloring', vertex.label = paste0(V(g)$name, '/', V(g)$weight, '/', V(g)$color))
  }

  return (list(V1,V2,V3))
}