# Layouts -----



#' Title
#'
#' @param graph 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' graph         <- elite.network(den.culture, result = "affil")
#' plot(layout.cl(graph, decompose = TRUE))
layout.cl                      <- function(graph, decompose = TRUE, n = 3, remove.isolates = FALSE, ...){
    graph.out                     <- as.undirected(graph, mode = "collapse")
    graph.out                     <- largest.component(graph.out)
    g                             <- graph.out
    
    if (identical(decompose, TRUE)) {
    g                             <- betweenness.decomposition(graph, max.path = 2, estimate.min = 1)
    }
    
    sp                            <- distances(g) <= n
    g.sp                          <- graph.adjacency(sp, mode = "undirected")
    
    cl.sp                         <- cluster_louvain(g.sp)
    cl.reduced                    <- as.factor(membership(cl.sp))
    cl.names.reduced              <- split(V(g.sp)$name, f = cl.reduced)
    
    
    cl.reduced.mem                 <- rep(NA, length = vcount(graph.out))
    for (i in 1:length(cl.names.reduced)) cl.reduced.mem[V(graph.out)$name %in% cl.names.reduced[[i]]] <- i
    
    o                              <- drl_defaults$default
    o$edge.cut                     <- 0.8
    o$init.iterations              <- 5
    o$init.attraction              <- 0.2
    o$expansion.temperature        <- 1000
    o$crunch.iterations            <- 100
    o$crunch.attraction            <- 0.5
    o$crunch.temperature           <- 500
    o$expansion.iterations         <- 300
    o$simmer.temperature           <- 50000
    o$simmer.attraction            <- 0.3
    o$cooldown.attraction          <- 0.5
    
    cl.fill                        <- cluster_louvain(graph.out, weights = 1/E(graph.out)$weight)
    clm                            <- as.factor(membership(cl.fill))
    split.names                    <- split(V(graph.out)$name, f = clm)
    sub.graphs                     <- lapply(split.names, induced_subgraph, graph = graph.out)
    
    edges                          <- lapply(sub.graphs, get.edgelist)
    edges                          <- do.call("rbind", edges)
    
    graph.plot                     <- graph.out
    E(graph.plot)$weight           <- 1/E(graph.plot)$weight
    
    for (i in 1:nrow(edges)) {
      E(graph.plot, edges[i, ])$weight <- E(graph.plot, edges[i, ])$weight * 2
    }
    
    # Reduced graph edges
    
    clm                            <- cl.reduced.mem
    split.names                    <- split(V(graph.out)$name, f = clm)
    sub.graphs                     <- lapply(split.names, induced_subgraph, graph = graph.out)
    
    edges                          <- lapply(sub.graphs, get.edgelist)
    edges                          <- do.call("rbind", edges)
    
    
    for (i in 1:nrow(edges)) {
      E(graph.plot, edges[i, ])$weight <- E(graph.plot, edges[i, ])$weight * 2
    }
    
    gs                             <- graph.strength(graph.out)
    lay                            <- cbind(gs, degree(graph.out))
    lay                            <- layout_with_drl(graph.plot, weights = E(graph.plot)$weight,  options = o, seed = lay)
    lay                            <- layout_with_fr(graph.plot, coords = lay, niter = 4, start.temp = 15)
    
    if (identical(remove.isolates, FALSE)) {
      lay.isolates                   <- add_isolates(graph, graph.out, lay = lay)
      lay.isolates[, 1]              <- lay.isolates[, 1] * -1
      lay                            <- lay.isolates
    }
    
    lay
}

add_isolates                    <- function(graph, graph.out, lay, stor = 1.1, afstand = 0.2){
  
  lay.stjernen                   <- lay
  lay.stjernen                   <- norm_coords(lay.stjernen)
  
  lay.all.o                      <- layout_with_fr(graph, weights = 1/E(graph)$weight, grid = "nogrid", niter = 500)
  ind                            <- V(graph)$name %in% V(graph.out)$name
  
  lay.all                        <- norm_coords(lay.all.o)
  lay.all[ind,]                  <- lay.stjernen
  lay.all[ind == FALSE,]         <- (lay.all[ind == FALSE,] * stor) + afstand
  
  lay.all
}


layout.elite <- function(graph, ...){
  
}

layout.isolates.in.box <- function(graph, layout, ...){
  
}
