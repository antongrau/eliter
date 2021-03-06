# Graph creation ----

#' Adjacency matrix for individuals
#' 
#' Create an adjacency matrix from an affilation list
#' @param den an affiliation list
#' @return a sparse adjacency matrix of individual * individual
#' @export
#' @examples 
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' adj.ind(den.culture)

adj.ind <- function(den){
  incidence       <- xtabs(~ NAME + AFFILIATION, data = den, sparse = TRUE)
  adj             <- incidence %*% Matrix::t(incidence)
  adj
}

#' Adjacency matrix for affiliations
#' 
#' Create an adjacency matrix from an affilation list
#' @param den an affiliation list
#' @return a sparse adjacency matrix of affiliation * affiliation
#' @seealso \link{adj.ind}, \link{two.mode}
#' @export
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' adj.affil(den.culture)


adj.affil <- function(den){
  incidence       <- xtabs(~ NAME + AFFILIATION, data = den, sparse = TRUE)
  adj             <- Matrix::t(incidence) %*% incidence
  adj
}

#' Create a two-mode network from an affiliation list
#' 
#' @param den an affiliation list
#' @export

two.mode <- function(den){
  graph            <- graph.data.frame(den[, c("NAME", "AFFILIATION")])
  V(graph)$type    <- V(graph)$name %in% unique(den$AFFILIATION)
  per              <- invPerm(order(V(graph)$type, V(graph)$name))
  graph            <- permute.vertices(graph, per)
  graph
}


#' Create a weighted network graph
#'
#' @param den a den class object, see \link{as.den}
#' @param result the type of returned graph
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#' @return a weighted graph
#' @export
#'
#' @examples
#' data(den)
#' den.pol       <- has.tags(den, "Politics", result = "den")
#' elite.network(den.pol)
#' elite.network(den.pol, result = "affil")
#' elite.network(den.pol, result = "two.mode")
elite.network      <- function(den, result = c("ind", "affil", "two.mode"), sigma = 14, base = exp(1)){
  
  result.args      <- c("ind", "affil", "two.mode")
  
  if (match.arg(result, result.args) == "ind") {
    graph          <- eliter:::elite.network.ind(den, sigma = sigma, base = base)
  }
  
  if (match.arg(result, result.args) == "affil") {
    graph          <- eliter:::elite.network.affil(den, sigma = sigma, base = base)
  }
  
  if (match.arg(result, result.args) == "two.mode") {
    graph          <- eliter:::elite.network.two.mode(den, sigma = sigma)
  }
  
 graph 
}

#' Elite network for individuals
#'
#' Construct a weighted elite network
#' @param den an affiliation edge list, in the \link{as.den} format.
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#' @return a weighted igraph graph
#' @examples 
#' data(den)
#' den.health <- has.tags(den, "Health", result = "den")
#' elite.network(den.health)

elite.network.ind    <- function(den, sigma = 14, base = exp(1)){
  
  # Incidence matrix
  incidence               <- xtabs(formula = ~ NAME + AFFILIATION, data = den, sparse = TRUE)
  affil.members           <- Matrix::colSums(incidence)
  memberships             <- Matrix::rowSums(incidence)
  
  # Occassions weight
  col.max                 <- as.numeric(qlcMatrix::colMax(incidence))
  incidence               <- Matrix::t(Matrix::t(incidence) * (1 / col.max))
  
  # Affiliation size weight
  affil.weight                    <- sqrt((sigma/affil.members))
  affil.weight[affil.weight > 1]  <- 1
  names(affil.weight)             <- colnames(incidence)
  
  incidence              <- Matrix::t(incidence) * affil.weight
  incidence              <- Matrix::t(incidence)
 
  # Adjacency matrix
  adj.all                <- Matrix::tcrossprod(sqrt(incidence), sqrt(incidence))
  #adj.all               <- sqrt(incidence) %*% sqrt(Matrix::t(incidence)) # Old version
  weighted.memberships   <- Matrix::diag(adj.all)
  
  # Graph creation
  graph                  <- graph.adjacency(adj.all, weighted = TRUE, diag = FALSE, mode = "undirected")
  
  # Weighting of strong ties
  over                            <- which(E(graph)$weight > 1)
  E(graph)$weight[over]           <- log(E(graph)$weight[over], base = base) + 1
  E(graph)$weight                 <- 1/E(graph)$weight
  
  # Attributes
  V(graph)$weighted.memberships   <- weighted.memberships
  V(graph)$memberships            <- memberships
  graph$affil.weight              <- affil.weight
  graph$affil.members             <- affil.members
  
  class(graph) <- c("igraph", "elite.network")
  graph
}

#' Elite network for affiliations
#'
#' Construct a weighted elite network of affiliations
#' @param den an affiliation edge list in the \link{as.den} format.
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#' @return a elite network object

elite.network.affil      <- function(den = den, sigma = 14, base = exp(1)){
  
  incidence              <- xtabs(formula = ~ NAME + AFFILIATION, data = den, sparse = TRUE)
  
  # Occassions weight
  col.max                <- as.numeric(qlcMatrix::colMax(incidence))
  incidence              <- Matrix::t(Matrix::t(incidence) * (1 / col.max))

  adj.affil              <- Matrix::crossprod(incidence)
  affil.members          <- Matrix::diag(adj.affil)
  
  # Affiliation size weight
  affil.weight                     <- sqrt((sigma/affil.members))
  affil.weight[affil.weight > 1]   <- 1
  names(affil.weight)              <- colnames(incidence)
  
  adj.affil                        <- adj.affil * affil.weight
  graph                            <- graph.adjacency(adj.affil, weighted = TRUE, diag = FALSE, mode = "directed")
  V(graph)$members                 <- affil.members
  V(graph)$weighted.members        <- Matrix::diag(adj.affil)
  
  over                             <- E(graph)$weight > 1
  E(graph)$weight[over]            <- log(E(graph)$weight[over], base = base) + 1
  E(graph)$weight                  <- 1/E(graph)$weight
  graph
}


#' A weighted two mode network
#'
#' @param den a den class object
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#'
#' @return an igraph graph
elite.network.two.mode             <- function(den, sigma = 14){
  
  incidence                        <- xtabs(formula = ~ NAME + AFFILIATION, data = den, sparse = TRUE)
  
  # Occassions weight
  col.max                          <- as.numeric(qlcMatrix::colMax(incidence))
  
  incidence                        <- Matrix::t(Matrix::t(incidence) * (1 / col.max))
  
  # Size weight
  affil.members                    <- Matrix::colSums(incidence)
  affil.weight                     <- sqrt((sigma/affil.members))
  affil.weight[affil.weight > 1]   <- 1
  names(affil.weight)              <- colnames(incidence)
  
  incidence                        <- incidence * affil.weight

  # Graph creation  
  graph                            <- graph.incidence(incidence, weighted = TRUE, mode = "all", multiple = FALSE)
  
  # Graph attributes
  edge.vars           <- c("ROLE", "TAGS")
  edge.vars           <-  edge.vars[edge.vars %in% colnames(den)]
  if (length(edge.vars) >= 1) {
    dups              <- which(duplicated(data.frame(den$NAME, den$AFFILIATION)))
    edge.att          <- den[-dups, c("NAME", "AFFILIATION", edge.vars)]
    graph.edges       <- get.edgelist(graph)
    graph.edges       <- data.frame("NAME" = graph.edges[,1], "AFFILIATION" = graph.edges[,2], stringsAsFactors = FALSE)
    joined            <- left_join(graph.edges, edge.att, by = c("NAME", "AFFILIATION"))
    for (i in 1:length(edge.vars)) edge_attr(graph, edge.vars[i])  <- as.character(joined[, edge.vars[i]])
  }
  
  graph
}

#' A report of the first neighborhood of an individiual
#' @param name a single or several names
#' @param den an affiliation edge list in the \link{den} format
#' @param n the maximum social distance - or highest edge weight - that a person can be from the individuals in name
#' @param text if "affil" only affiliations are given labels
#' @param member.of is a set of names of individuals or affiliations that are used for determining the fill of the vertices.
#' @param ... all other arguments are passed on to \link{graph.plot.twomode}
#' @return a ggplot ego.plot
#' @export

ego.two.mode <- function(name, den = den, n = Inf, text = "affil", member.of = pe13$Name, ...){
  ind       <- den$NAME %in% name  
  stopifnot("Wrong name" = any(ind))
  
  affil        <- as.character(den$AFFILIATION[ind])
  den.affil    <- den[den$AFFILIATION %in% affil,]
  
  if (n < Inf) {
    net.elite    <- elite.network(den.affil)
    ind.e        <- V(net.elite)$name %in% name
    dist.to.ind  <- net.elite[ind.e,]
    ind.e        <- V(net.elite)$name[dist.to.ind < n]
    den.affil    <- den.affil[den.affil$NAME %in% ind.e,]
  }
  
  net.two      <- two.mode(den.affil)
  elite.net    <- elite.network(as.den(den.affil))
  name.in.net  <- which(V(elite.net)$name %in% name)
  sp.ego       <- shortest.paths(elite.net, v = name.in.net)
  sp.ego       <- 1/sp.ego 
  sp.ego[name.in.net] <- 2
  sp.ego[name.in.net] <- max(sp.ego) + 1
  affil.w.ego    <- elite.net$affil.weight
  V(net.two)$ego.dist <- c(sp.ego, affil.w.ego)
  
  
  type         <- V(net.two)$type
  type[V(net.two)$name %in% name] <- "Ind"
  type <- as.factor(type)
  levels(type)    <- c("Individual", "Ego", "Affiliation")
  
  member.of.TF    <- V(net.two)$name %in% member.of
  
  aff <- which(type == "Affiliation")
  
  e.w <- rep(1, ecount(net.two))
  E(net.two)$e.w <- e.w
  for (i in 1:length(aff))  E(net.two)[incident(net.two, aff[i])]$e.w <- affil.w.ego[i]
  
  if (identical(text, "affil")) {
    text       <- V(net.two)$name
    text[type == "Individual"] <- NA
  }
  
  E(net.two)$weight <- E(net.two)$e.w
  
  p <- graph.plot.twomode(net.two, text = text, vertex.fill = member.of.TF, vertex.size = V(net.two)$ego.dist, edge.color = "black", edge.alpha = E(net.two)$e.w, vertex.shape = type, edge.size = 0.45, ...)
  p <- p + scale_fill_manual(values = c("white", "black"), guide = "none") + scale_shape_manual(values = c(21, 13, 23 ), guide = "none") + scale_alpha_continuous(range = c(0.08, 0.4), guide ="none") + scale_size_continuous(range = c(2, 4), guide = "none")
  p + ggtitle(name)
}

#' Ego network
#' 
#' An ego network of individuals
#' @param name
#' @param den
#' @param n
#' @return a ggplot2 plot
#' @export

ego.network      <- function(name, den, n = Inf){
  ind            <- den$NAME %in% name  
  stopifnot("Wrong name" = any(ind))
  affil          <- as.character(den$AFFILIATION[ind])
  den.affil      <- den[den$AFFILIATION %in% affil,]
  net.elite      <- elite.network(den.affil)
  ind.e          <- V(net.elite)$name %in% name
  dist.to.ind    <- net.elite[ind.e,]
  delete.them    <- which(dist.to.ind >= n & ind.e == FALSE)
  net.elite      <- delete.vertices(net.elite, delete.them)
  
  ego.name       <- V(net.elite)$name
  ego.name[(V(net.elite)$name %in% name) == FALSE] <- ""
  V(net.elite)$ego.name   <- ego.name
  V(net.elite)$ego.degree <- net.elite[V(net.elite)$name %in% name,]
  V(net.elite)$ego.degree
  net.elite
}



#' A report of the first neighborhood of an affiliation
#' @param name a single or several affilation names
#' @param den an affiliation edge list in the \link{den} format
#' @param text if "affil" only affiliations are given labels
#' @param member.of is a set of names of individuals or affiliations that are used for determining the fill of the vertices.
#' @param ... all other arguments are passed on to \link{graph.plot.twomode}
#' @return a ggplot ego.plot
#' @export

ego.two.mode.affil <- function(name, den = den, text = "affil", member.of = pe13$Name, text.background = "white", ...){
  
  aff          <- den$AFFILIATION %in% name
  
  stopifnot("Wrong name" = any(aff))
  inds         <- as.character(den$NAME[aff])
  den.affil    <- den[den$NAME %in% inds,]  
  
  net.two      <- two.mode(den.affil)
  
  type         <- V(net.two)$type
  type[V(net.two)$name %in% name] <- "Ind"
  type <- as.factor(type)
  levels(type)    <- c("Individual", "Ego", "Affiliation")
  
  member.of.TF    <- V(net.two)$name %in% member.of
  
  aff <- which(type == "Affiliation")
  
  E(net.two)$weight <- rep(1, ecount(net.two))
  #   E(net.two)$e.w <- e.w
  
  if (identical(text, "affil")) {
    text       <- V(net.two)$name
    text[type == "Individual"] <- NA
  }
  
  # E(net.two)$weight <- E(net.two)$e.w
  
  p <- graph.plot.twomode(net.two, layout = layout_with_fr(graph, grid = "nogrid"),  text = text, vertex.fill =  member.of.TF, vertex.size = degree(net.two), edge.color = "black", vertex.shape = type, edge.size = 0.45, text.background = text.background, ...)
  p <- p + scale_fill_manual(values = c("white", "black", "black"), guide = "none") + scale_shape_manual(values = c(21, -0x25C9, 23 ), guide = "none") + scale_alpha_continuous(range = c(0.08, 0.4), guide = "none") + scale_size_continuous(range = c(2, 4), guide = "none")
  p + ggtitle(name)
}




#' Create a reduced graph from a list of den objects
#'
#' Creates a reduced graph on the basis of odds-ratios. Note that if the den objects are not mutually exclusive it may influence the interpretation of the odds-ratio edges.
#'
#' @param list.dens a list of den objects, like those created by \link{tags.to.sectors}.
#'
#' @return an igraph object with additional attributes
#' @export
#'
#' @examples

reduced.sector.graph    <- function(list.dens){
  sector.names          <- names(list.dens)
  
  edge.klynge           <- list()
  for(i in 1:length(sector.names)) edge.klynge[[i]] <- data.frame(NAME = unique(list.dens[[i]]$NAME), AFFILIATION = sector.names[i])
  edge.klynge           <- do.call("rbind", edge.klynge)
  incidence             <- xtabs(~NAME + AFFILIATION, edge.klynge, sparse = TRUE)
  adj                   <- Matrix::crossprod(incidence)
  
  r                     <- adj / Matrix::diag(adj)
  a                     <- adj
  diag(a)               <- 0
  network.size          <- sum(a)/2
  
  odds.mat              <- (a / network.size) * sum(1:ncol(adj)-1)
  diag(odds.mat)        <- NA
  adj.klynge.pp         <- odds.mat
  
  adj.klynge.pp         <- as.data.frame(as.matrix(adj.klynge.pp))
  colnames(adj.klynge.pp) <- rownames(adj.klynge.pp)
  
  adj.klynge.pp[adj.klynge.pp < 1] <- 0
  reduced.graph         <- graph.adjacency(as.matrix(adj.klynge.pp), diag = FALSE, weighted = TRUE)
  
  E(reduced.graph)$size          <- suppressMessages(adj[reduced.graph[,] != 0])
  reduced.graph$original.matrix  <- adj
  reduced.graph$odds.matrix      <- odds.mat
  
  reduced.graph
  
}