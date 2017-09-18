# Mål at lave en reach graph - i bidder


library(eliter)
data(den)

graph.all  <- elite.network(den)
graph.link <- betweenness.decomposition(largest.component(graph.all)) 

graph <- graph.link
order <- 2.1
verbose <- TRUE

reach.in.chunks <- function(graph, order = 2.1, verbose = TRUE){
  sparse        <- Matrix(0, nrow = vcount(graph), ncol = vcount(graph), sparse = TRUE, dimnames = list(V(graph)$name, V(graph)$name))  
  d             <- 1:vcount(graph)
  chunks        <- split(d, sort(d %% 4))
  
  chunk.row <- chunks[[1]]
  chunk.col <- chunks[[2]]
  
distance.in.cell <- function(graph, chunk.row, chunk.col, order){
  sp.cell        <- distances(graph, v = chunk.row, to = chunk.col)
  sp.cell[sp.cell > order] <- 2.1
  Matrix(sp.cell)
}

distance.in.row <- function(graph, chunk.row, chunks, order){
  distance.rows <- llply(chunks, distance.in.cell, graph = graph, chunk.row = chunk.row, order = order)
  do.call(cbind, distance.rows)
}

distance.rows  <- llply(chunks, distance.in.row, graph = graph, chunks = chunks, order = order, .progress = "text")
do.call(rbind, distance.rows)
}

nif <- reach.in.chunks(graph.link)
naf <- distances(graph.link)
reach <- naf <= 2.1

ce <- closeness.estimate(graph.link, cutoff = 2.1, normalized = FALSE)


be <- betweenness.estimate(graph.link, cutoff = 2.1)
rs <- rowSums(reach)

summary(rs)




g  <- find.core(naf[-which(rs < median(rs)), -which(rs < median(rs))])
g  <- find.core(naf[-which(ce < quantile(ce)[3]), -which(ce < quantile(ce)[3])])


te <- ce * vcount(graph.link)


te
rs


naf[-which(ce < quantile(ce)[3]), -which(ce < quantile(ce)[3])]


graph.ce   <- graph.link - which(ce < quantile(ce)[3])
sp.ce      <- distances(graph.ce)
core.ce    <- find.core(sp.ce)
tail(table(core.ce), 20)
length(core.ce)

cor(tail(table(core.ce), 100), tail(table(gf), 100))


tail(table(gf), 20)


table(g)

gf <- find.core(naf)

tail(table(gf))
tail(table(g))
find.core(g)

rs[1]
ce[1]

cor(ce, rs)
dist.plot(ce)
dist.plot(rs)


# Centrality core -----

centrality.fun   <- function(x) closeness.estimate(x, cutoff = 3)
centraliy.core   <- function(graph, start.level = 0, verbose = FALSE, centrality.fun = graph.strength, ...){
  
  rnormalize      <- function(x){
  x               <- 1/x
  x               <- x/mean(x)
  x               <- x/min(x)
  x               <- round(x, digits = 0)
  x
  }
  
  E(graph)$weight <- rnormalize(E(graph)$weight)
  
  #adj             <- get.adjacency(graph, attr = "weight", sparse = TRUE)
  
  level.down     <- function(x, level){
    g            <- x
    #gs           <- graph.strength(g)
    gs           <- centrality.fun(g, ...)
    
    while (any(gs <= level)) {
      delete      <- which(gs <= level)
      g            <- g - delete
      gs           <- centrality.fun(g)  
    }
    setdiff(V(x)$name, V(g)$name)
  }
  
  g               <- graph
  k.score         <- 0
  k.vector        <- rep(Inf, vcount(graph)) 
  gs              <- centrality.fun(g, ...)
  minimum.degree  <- start.level
  
  while (k.score <= minimum.degree & vcount(g) != 0) {
    #while (vcount(g) > 0){
    candidate.names <- level.down(g, level = minimum.degree)
    candidates      <- which(V(graph)$name %in% candidate.names)
    k.vector[candidates] <- k.score
    k.score         <- k.score + 1
    delete          <- which(V(g)$name %in% candidate.names)
    g               <- g - delete
    if (vcount(g) == 0) break
    gs              <- Matrix::rowSums(g)
    minimum.degree  <- min(gs)
    if (identical(verbose, TRUE)) cat("Minimum degree: ", minimum.degree, "Removed: ", length(candidate.names), "Remain: ", vcount(g), "\n")
  }
  
  k.vector + start.level
}


