# Centrality ---

#' Reach centrality
#' 
#' The number of people you can reach within a few steps (usually 2). This is 
#' similar to the size of your neighborhood except that reach takes the strength
#' of the edges into account. The distance between two people is the sum of the
#' edge weights of the edges in the path between the two people. If you use the
#' max.path argument you can restrict the maximum number of people a "reach
#' path" can go through. This is relevant for networks with very strong
#' connections aka. short distances between agents.
#' 
#' Reach can be calculated for both directed and undirected graphs. For
#' unweighted graphs see \link{neighborhood.size} or \link{ego.size} from
#' \link{igraph}.
#' 
#' 
#' @param graph a weighted graph
#' @param order the maximum distance between two vertices
#' @param mode the direction of the edges; either, "in", "out" or "all"
#' @param weight the edge weight as distance
#' @param max.path the maximum unweighted path length
#'   
#' @return a numeric vector with reach scores
#' @seealso ego, ego_size, degree, vertex.measures
#' @export
#' 
#' @examples
#' data(pe13)
#' r <- reach(net.elite, order = 2)
#' tail(sort(r))
#' r <- reach(net.elite, order = 3, max.path = 2)
#' tail(sort(r))

reach            <- function(graph, order = 2, mode = "all", weight = E(graph)$weight, max.path = NULL){
  g              <- graph
  E(g)$weight    <- weight
  g              <- delete.edges(g, edges = which(E(g)$weight > order))
  sp             <- distances(g, mode = mode, weights = E(g)$weight)
  
  if(is.numeric(max.path)){
  sp.mp                <- distances(g, mode = mode, weights = NA)
  sp[sp.mp > max.path] <- Inf
  }
  
  rowSums(sp <= order)
}


#' Do you know?
#' 
#' Find out how well members of a network knows a group of people. This is very
#' much like \link{degree} or \link{strength} where you only count connections
#' to people within a selected group.
#' 
#' @param graph a igraph network object created with the \link{elite.network}
#'   function.
#' @param you a character vector of names present in graph
#' @param people a character vector of names preferably present in graph
#' @param how.well a number that says how weak the weakest considered edge is.
#'   Edge weight is considered social distance and there higher is weaker.
#' @return a numeric vector with the \link{graph.strength} of the individuals
#'   named in "you". The graph strength is the sum of weighted edges within the
#'   group "people".
#' @export
#' @seealso reach, vertex.measures, graph.strength, degree
#' @examples
#' library(soc.elite)
#' data(den)
#' data(pe13)
#' graph         <- elite.network(den)
#' you           <- pe13$Name
#' people        <- has.tags(den, tags = c("Political party"))
#' do.you.know(graph, you, people, how.well = 2)

do.you.know <- function(graph, you, people, how.well = 1){
  
  people.position    <- which(V(graph)$name %in% people)
  
  people.in.your.hood <- function(graph, your.name, people.position, how.well = 1){
    your.name.position           <- which(V(graph)$name %in% your.name)
    you.and.your.people.position <- unique(c(your.name.position, people.position))
    people.graph                 <- induced.subgraph(graph, you.and.your.people.position)
    you.in.people.graph          <- which(V(people.graph)$name %in% your.name)
    people.graph                 <- delete.edges(graph = people.graph, edges = which(E(people.graph)$weight > how.well))
    graph.strength(people.graph, vids = you.in.people.graph, weights = 1/E(people.graph)$weight, loops = FALSE)
  }
  score        <- sapply(you, people.in.your.hood, graph = graph, people.position = people.position, how.well = how.well)
  names(score) <- you
  score
}

#' Vertex centrality measures
#' 
#' The classical centrality measures; degree, weighted degree, closenesss,
#' reach, betweenness and Burts constraint. This function is useful when you want to export results.
#' @param graph is an \link{igraph} network object
#' @param order is the maximum distance between two individuals for the reach
#'   statistic
#' @return a data.frame with a lot of descriptives
#' @examples
#' data(pe13)
#' vertex.measures(net.elite)
#' @export

vertex.measures <- function(graph, mode = "all", order = 2.1){
  
  memberships       <- V(graph)$memberships
  l                 <- vcount(graph) + 1
  gs                <- graph.strength(graph, mode = mode, weights = 1/E(graph)$weight)
  g                 <- delete.edges(graph, edges = which(E(graph)$weight > 1))
  deg               <- degree(g)
  
  
  reach.value       <- reach(graph, order = order, mode = mode)
  reach.rank        <- l - rank(reach.value)
  
  close.w           <- closeness(graph, mode = mode)
  close.rw          <- l - rank(close.w)
  
  between.w         <- round(betweenness(graph), 1)
  between.rw        <- l - rank(between.w)
  
  constraint        <- round(constraint(graph) * 1000)
  
  out               <- data.frame("Memberships" = memberships,
                                  "Degree"      = degree(graph),
                                  "Degree <= 1" = deg,
                                  "Weighted degree" = round(gs, 1),
                                  "Reach" = reach.value,
                                  "Reach rank" = reach.rank,
                                  "Closeness rank" = close.rw,
                                  "Betweenness" = between.w,
                                  "Betweenness rank" = between.rw,
                                  "Burts constraint" = constraint,
                                  check.names = F)
  
  out
}

#' Vertex descriptives for directed graphs
#' 
#' Descriptive statistics for each vertex
#' @param net is an directed \link{igraph} network object
#' @param reach is the maximum distance between two individuals for the reach statistic
#' @return a matrix with a lot of descriptives
#' @export


vertex.measures.directed <- function(net, n = 2.5){
  
  sp.in               <- shortest.paths(net, mode = "in")
  sp.out              <- shortest.paths(net, mode = "out")
  av.path.length      <- rowSums(sp.in) / nrow(sp.in)
  l                 <- vcount(net) + 1
  deg.in            <- rowSums(sp.in <= 1)
  deg.out           <- rowSums(sp.out <= 1)
  deg.all           <- (deg.in + deg.out)/2
  close.in          <- closeness(net, mode = "in", normalized = TRUE)
  close.out         <- closeness(net, mode = "out", normalized = TRUE)
  close.rin         <- l - rank(close.in)
  close.rout        <- l - rank(close.out)
  between.w         <- round(betweenness(net), 1)
  between.rw        <- l - rank(between.w)
  n2.in             <- rowSums(sp.in <= n)
  n2.out            <- rowSums(sp.out <= n)
  n2.rin            <- l - rank(n2.in)
  n2.rout           <- l - rank(n2.out)
  page.rank         <- round(page.rank(net)$vector * 1000)
  
  out               <- data.frame(deg.in, deg.out, deg.all, n2.in, n2.out, n2.rin, n2.rout,
                                  close.rin, close.rout, between.w, between.rw, av.path.length, page.rank)
  colnames(out)     <- c("In degree", "Out degree", "All degrees", "Reach in", "Reach out", "Reach in rank", "Reach out rank", 
                         "Closeness in rank", "Closeness out rank", "Betweenness", "Betweenness rank", "Average path length", "Pagerank")
  out
}

# data(pe13)
# graph <- net.elite
# sectors <- tags.to.sectors(den, standard.sectors("Danish"))
# how.well <- 2
# you = V(graph)$name

#' Title
#'
#' @param graph 
#' @param you 
#' @param sectors 
#' @param how.well 
#'
#' @return
#' @export
#'
#' @examples
sector.connections  <- function(graph, you = V(graph)$name, sectors, how.well = 2){
  #stopifnot(all(sapply(sectors, is.den)))
  sector.names      <- lapply(sectors, getElement, name = "NAME")
  sector.names      <- lapply(sector.names, unique)
  
  sector.know       <- lapply(sector.names, FUN = do.you.know, graph = graph, you = you, how.well = how.well)
  sector.mat        <- do.call("cbind", sector.know)  
  sector.mat
}
