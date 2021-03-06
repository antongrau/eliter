# Core identification ----

#' Largest component
#'
#' Finds the largest component in a network.
#' @param graph a \link{igraph} network object
#' @param cut.off is the minimum number of weighted affiliation memberships a component member has to have.
#' @param result either a graph with the largest component or a logical vector.
#' @return a graph object
#' @export
#' @examples
#' data(den)
#' den.health   <- has.tags(den, "Health", res = "relations")
#' graph.health <- elite.network(den.health)
#' largest.component(graph.health)

largest.component <- function(graph, cut.off = 1, result = c("graph", "vector")){
  is.elite.network    <- inherits(graph, "elite.network") #! This does not work - becuz igraph removes classes.
  original.names      <- V(graph)$name

  ind                 <- vector()
  if (inherits(graph, "elite.network")) {
    ind               <- c(ind, which(V(graph)$weighted.memberships <= cut.off))
    graph             <- graph - ind
  }

  cl                  <- clusters(graph)
  ind                 <- which(cl$membership != which.max(cl$csize))

  ind                 <- unique(ind)
  graph.com           <- graph - ind
  graph.com$removed.vertex  <- original.names %in% V(graph.com)$name
  if (is.elite.network)  class(graph.com)    <- c("igraph", "elite.network")
  out                 <- graph.com

  if (identical(match.arg(arg = result, choices = c("graph", "vector")), "vector")) {
  out                 <- original.names %in% V(graph.com)$name
  }

  out
}

#' Betweenness decomposition
#'
#' Find the people that link the network together. All individuals with less than the minimum betweenness are removed from the network.
#' This removes "hangers" and people with connections that are perfectly similar to others. Betweenness.decomposition is useful before core identification and as a pruning tool for "dirty" data.
#' If you choose a relatively high minimum betweenness estimate then it can serve as a core identification method.
#'
#' @param graph a network graph
#' @param max.path the longest paths used by \link{betweenness.estimate}
#' @param estimate.min
#'
#' @return a graph
#' @export
#'
#' @examples
#' data(pe13)
#' betweenness.decomposition(net.elite)

betweenness.decomposition <- function(graph, max.path = 2, estimate.min = 1, silent = FALSE){
  g                       <- graph
  layers                  <- list()
  i                       <- 1
  
  if (is.function(estimate.min) == FALSE) min.fun <- function(x) estimate.min
  if (is.function(estimate.min)) min.fun <- estimate.min
  
  while (any(betweenness.estimate(g, cutoff = max.path) < min.fun(g))) {
    kill        <- which(betweenness.estimate(g, cutoff = max.path) < min.fun(g))
    layers[[i]] <- V(g)[kill]
    i           <- i + 1
    g           <- g - kill
  }
  
  if (identical(silent, TRUE)) {
    out           <- unlist(lapply(layers, length))
    names(out)    <- paste("Round:", seq_along(out))
    print(out)
  }
  
  g
}





#' Find the core in an elite network
#'
#' Uses the k-core decomposition, see \link{graph.adjacency}, to identify the elite
#' @param sp a shortest paths matrix
#' @param reach the maximum distance considered as a relation in the decompostition
#' @return a numerical vector with the coreness score for each vertex
#' @export
#' @examples
#' data(den)
#' health.affil  <- has.tags(den, c("Health"))
#' den.health    <- den[den$AFFILIATION %in% health.affil,]
#' graph.health  <- elite.network(den.health)
#' sp.health     <- shortest.paths(graph.health)
#' core.health   <- find.core(sp.health)
#' table(core.health)

find.core <- function(sp, reach = 2.1){
  sp.1     <- (sp <= reach) * 1
  net.sp   <- graph.adjacency(sp.1, mode = "undirected", diag = FALSE, weighted = TRUE)
  core     <- graph.coreness(net.sp)
  core
}

#' Secondary actors
#'
#' Identify secondary actors within a group. A secondary actor is an individual with a neighborhood that is perfectly nested within the neighborhood of another individual.
#' Here it is identified by comparing memberships between all agents within a group. If any individual has the exact same memberships as another individual he is considered a secondary actor.
#' See Faust, Katherine. “Centrality in Affiliation Networks.” Social Networks 19, no. 2 (1997): 157–191. for considerations on the exclusion of secondary actors.
#' Consider betweenness decomposition as it is a more general solution to the same problem, see \link{betweenness.decomposition}
#' @param x a named core numerical vector with coreness values, see \link{graph.coreness}
#' @param rel.all an affiliation edge list
#' @return a character vector with all the individuals with similar affiliation memberships seperated by "|". Unique actors return "FALSE"
#' @export
#' @examples
#' example(find.core)
#' secondary.actors(core.health, den.health)
secondary.actors <- function(x, rel.all){

  mem        <- names(x)[x == max(x)]
  rel.x      <- droplevels(rel.all[rel.all$NAME %in% mem,])

  affil      <- table(rel.x$NAME, rel.x$AFFILIATION)
  affil      <- affil > 0
  mem.list   <- apply(affil, 1, which)
  overlap    <- function(x, y) length(intersect(x,y)) == length(x)
  secondary  <- vector(length = length(mem.list))

  for (i in 1:length(mem.list)) {
    ov         <- which(sapply(mem.list, overlap, x = mem.list[[i]]))
    if (length(ov) > 1)  secondary[i] <- paste(unique(c(mem[i], mem[ov])), collapse = "|")
  }
  secondary
}

# NB! Social proximity is not working properly and it was written a very long
# time ago and is extremely slow. Apparrently values can be negative which is
# logically impossible.

#' Social proximity
#'
#' Calculates the social proximity of all vertices in a graph as described by
#' Alba and Kadushin:
#' Alba, Richard D., and Charles Kadushin. “The Intersection
#' of Social Circles: A New Measure of Social Proximity in Networks.”
#' Sociological Methods & Research 5, no. 1 (August 1, 1976): 77–102.
#'
#' @param graph is a \link{igraph} network
#' @param neihborhood a numerical value indicating the order of the
#'   neighborhood, see \link{neighborhood}
#' @param mode if "total" the proximity is calculated on the size of the
#'   combined neighborhood. If "own" or "other" proximity is calculated on the
#'   basis of either of the vertices in a relation.
#' @return a matrix with proximity measures
#' @examples
#' data(den)
#' den.health       <- has.tags(den, "Health", res = "relations", silent = TRUE)
#' graph.health     <- elite.network(den.health)
#' graph.health     <- betweenness.decomposition(graph.health)
#' proximity(graph.health)

proximity <- function(graph, neighborhood = 2, mode = "total"){
  n2 <- neighborhood(graph, order = neighborhood)

  ###
  individual.hoodoverlap <- function(n2, individual, result=1){
    hood <- n2[[individual]]
    res <- vector(length=length(n2))
    for (j in 1:length(n2)){
      hood2 <- n2[[j]]
      # Andel af egne forbindelser man deler med hood2
      hood.size          <- length(hood) #-1
      hood2.size         <- length(hood2) #-1
      hood.overlap       <- sum(hood %in% hood2) - sum(hood2 == j)
      hood.total.size    <- hood.size + hood2.size - hood.overlap # NB er det her korrekt!


      overlap.total      <- hood.overlap/hood.total.size
      overlap.own        <- hood.overlap/hood.size
      overlap.other      <- hood.overlap/hood2.size
      ind.res <- c(overlap.total, overlap.own, overlap.other, hood.total.size, hood.overlap)

      res[j]       <- ind.res[result]
    }
    return(res)
  }

  ############# Resultater
  if (identical(mode, "total")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))

    pb <- txtProgressBar(min = 0, max = length(n2), style=3)

    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=1)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }

  if (identical(mode, "own")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))

    pb <- txtProgressBar(min = 0, max = length(n2), style=3)

    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=2)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }

  if (identical(mode, "other")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))

    pb <- txtProgressBar(min = 0, max = length(n2), style=3)

    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=3)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }

  if (identical(mode, "overlap")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))

    pb <- txtProgressBar(min = 0, max = length(n2), style=3)

    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=5)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }

  rownames(circle.mat) <- V(graph)$name
  colnames(circle.mat) <- V(graph)$name

  return(circle.mat)
}



#' Vertex communities
#'
#' Memberships for several community detection algorithms and a meta community structure based on all communities.
#'
#' @param graph a weighted graph from \link{igraph}
#' @param weight the edge weights. Larger values are stronger ties.
#'
#' @return a data.frame with community memberships
#' @export
#'
#' @examples
#' data(den)
#' den.corp        <- has.tags(den, "Corporation", res = "relations", silent = TRUE)
#' graph.corp      <- elite.network.org(den.corp)
#' graph.corp      <- as.undirected(graph.corp)
#' vc              <- vertex.communities(graph.corp)
#' head(vc)
#' table(table(vc$"Meta louvain"))

vertex.communities <- function(graph, weight = 1/E(graph)$weight){

  E(graph)$weight  <- weight
  fast.greedy      <- as.numeric(membership(cluster_fast_greedy(graph)))
  label.prop       <- as.numeric(membership(cluster_label_prop(graph)))
  louvain          <- as.numeric(membership(cluster_louvain(graph)))
  walktrap         <- as.numeric(membership(cluster_walktrap(graph)))

  cluster.frame    <- data.frame("Name"              = V(graph)$name,
                                 "Fast and greedy"   = fast.greedy,
                                 "Label propagation" = label.prop,
                                 "Louvain"           = louvain,
                                 "Walktrap"          = walktrap,
                                 check.names = FALSE)

 md                 <- melt(cluster.frame, id.vars = "Name")
 md$cluster         <- paste(md$variable, md$value)
 inc                <- xtabs(~Name + cluster, md, sparse = TRUE)
 g                  <- graph.adjacency(inc %*% Matrix::t(inc), mode = "undirected")
 meta.louvain       <- membership(cluster_louvain(g))

 cluster.frame$"Meta louvain" <- meta.louvain
 cluster.frame
}

between_louvain_cluster <- function(graph, max.path = 2, estimate.min = 1){
  
  
  
}



#' Levels of power
#'
#' @param x a numeric vector with coreness scores produced by \link{find.core}
#'
#' @return a factor with 4 levels
#' @export

levels.of.power <- function(x){
  var                                            <- vector(length = length(x))
  var[x == max(x)]                               <- "1. Power Elite"
  var[x < max(x) & x >= 2/3 * max(x)]            <- "2. Higher levels of Power"
  var[x < 2/3 * max(x) & x >= 1/3 * max(x)]      <- "3. Middle levels of Power"
  var[x < 1/3 * max(x)]                          <- "4. Lower levels of Power"
  as.factor(var)
}




#' K-shell decomposition
#'
#' @param graph a graph from igraph
#' @param start.level the decomposition process starts at this level or minimum degree
#' @param verbose if TRUE shows the progress
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' data(den)
#' graph.all <- elite.network(den)
#' graph.com <- largest.component(graph.all)
#' sp        <- distances(graph.com)
#' sp[sp >= 2.1] <- 0
#' graph.reach     <- graph_from_adjacency_matrix(Matrix(sp), mode = "undirected", weighted = TRUE)
#' shell <- k.shell(graph.reach, start.level = 0, verbose = TRUE)
#' table(shell)

k.shell   <- function(graph, start.level = 0, verbose = FALSE){
  
  # E(graph)$weight <- 1/E(graph)$weight
  # E(graph)$weight <- E(graph)$weight / mean(E(graph)$weight)
  # norm            <- E(graph)$weight / min(E(graph)$weight)
  # rnorm           <- round(norm, digits = 0)
  # E(graph)$weight <- rnorm
  
  rnormalize      <- function(x){
    x               <- 1/x
    x               <- x/mean(x)
    x               <- x/min(x)
    x               <- round(x, digits = 0)
    x
  }
  
  E(graph)$weight <- rnormalize(E(graph)$weight)
  
  adj             <- get.adjacency(graph, attr = "weight", sparse = TRUE)
  
  level.down     <- function(x, level){
    g            <- x
    #gs           <- graph.strength(g)
    gs           <- Matrix::rowSums(g)
    
    while (any(gs <= level) & length(gs) > level) {
      delete      <- which(gs <= level)
      g            <- g[-delete, -delete]
      gs           <- Matrix::rowSums(g)  
    }
    setdiff(rownames(x), rownames(g))
  }
  
  g               <- adj
  k.score         <- 0
  k.vector        <- rep(Inf, vcount(graph)) 
  gs              <- Matrix::rowSums(adj)
  
  if (start.level < min(gs)) start.level <- min(gs)
  
  minimum.degree  <- start.level
  
  
  while (k.score <= minimum.degree & nrow(g) != 0) {
    candidate.names <- level.down(g, level = minimum.degree)
    candidates      <- which(V(graph)$name %in% candidate.names)
    
    k.score         <- k.score + 1
    delete          <- which(rownames(g) %in% candidate.names)
    g               <- g[-delete, -delete]
    
    if (nrow(g) == 0) break
    gs              <- Matrix::rowSums(g)
    
    if (minimum.degree >= min(gs)) break
    
    minimum.degree  <- min(gs)
    
    k.vector[candidates] <- k.score
    if (identical(verbose, TRUE)) cat("Minimum degree: ", minimum.degree, "Removed: ", length(candidate.names), "Remain: ", nrow(g), "\n")
  }
  
  k.vector[is.infinite(k.vector)] <- k.score 
  
  k.vector + start.level
}

