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
  chunks        <- split(d, sort(d%%4))
  
distance.in.cell <- function(graph, chunk.row, chunk.col, order){
  sp.cell        <- distances(graph, v = chunk.row, to = chunk.col)
}
  
  
}