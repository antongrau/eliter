# Functions that go from a clean den to core analysis

# Result object with a matrix of linkers and attributes
# A list of weighted adjacency matrices
# spell graph

# Each step has a progress bar

# Input
# weighting fun <- is a weighting function for the weighted adjacency matrix
# active period
# steps ?

# Måske skal spell graphen subsettes?

# example dat.

# library(readr)
# library(eliter)
# library(tidyr)
# library(plyr)
# load("~/Downloads/rstudio-export(3)/den.clean.Rda")
# load("~/Downloads/rstudio-export(3)/graph.spell.clean.Rda")

#graph.spell <- graph.spell - 1:(vcount(graph.spell) * 0.7)


#' Get shell from adjacency matrix
#'
#' @param x an sparse adjacency matrix
#' @param start.level the first level of the k-shell decomposition
#' @param verbose if TRUE prints a lot of info
#'
#' @return
#' @export
#'
#' @examples
get.shell    <- function(x, start.level = 20, verbose = FALSE){
  g           <- graph_from_adjacency_matrix(x, mode = "undirected", weighted = TRUE)
  g           <- delete.edges(g, which(E(g)$weight > 2.1))
  g           <- delete.edges(g, which(E(g)$weight <= 0))

  # estimate.min.fun <- function(x){
  #   #o <- graph.strength(x)
  #   o <- knn(x, weights = 1/E(x)$weight)$knn
  #   t <- transitivity(x, type = "local")
  #   #names(t) <- names(o)
  #   #b <- betweenness.estimate(x, cutoff = 2)
  #   o <- (((o - 1) * t))
  #   #o <- 1 + (o - (o * t))
  #
  #   #o <- 1/(1 - transitivity(x, type = "local")[i])
  #   o[o < 1] <- 1
  #   o[is.na(o)] <- 1
  #   o
  # }

  estimate.min.fun <- function(x) graph.strength(x, weights = 1/E(x)$weight)
  dg.com      <- largest.component(g, cut.off = 0)

  # E(dg.com)$weight[E(dg.com)$weight < 1] <- 1 # Her capper den fuldstændigt betydningen af alle stærke forbindelser...

  dg.com      <- betweenness.decomposition(dg.com, max.path = 2, estimate.min = estimate.min.fun)

  dg.com      <- largest.component(dg.com, cut.off = 0)
  sp          <- distances(dg.com)
  sp.u        <- distances(dg.com, weights = NA)
  #cat("\n", "Linkers: ", vcount(dg.com), "\n")
  set         <- which(sp <= 2 & sp.u <= 2, arr.ind = TRUE)
  #set         <- which(sp <= 2, arr.ind = TRUE)
  spm         <- sparseMatrix(dims = dim(sp), i = set[, 1], j = set[, 2] , x = sp[set])
  spm         <- as(object = spm, "dgCMatrix")
  dimnames(spm) <- dimnames(sp)
  g.reach     <- graph_from_adjacency_matrix(spm, mode = "max", weighted = TRUE, diag = FALSE) #? Meget sært så

  core        <- NA
  names(core) <- NA
  
  if(ecount(g.reach) > 0){

  core        <- k.shell(g.reach, start.level = start.level, verbose = verbose)
  names(core) <- V(g.reach)$name
  }

  core
}


#' Identify dynamic cores
#'
#' @param den a den class object
#' @param graph.spell a graph.spell - if NULL it is calculated
#' @param adjacency.list a list of weighted adjacency matrices - if NULL it is calculated
#' @param period a vector with start date and end date, see \link{ymd}
#' @param median.term the median number of months a term lasts
#' @param max.terms the maximum number of terms
#' @param boost the minimum value of active relations
#'
#' @return
#' @export
#'
#' @examples
dynamic.core.analysis <- function(den, graph.spell = NULL, adjacency.list = NULL, period = c(ymd(19880101), ymd(19901231)), median.term = 38, max.terms = 2, boost = 38){

  # Spell graph
  if (is.null(graph.spell)){
  graph.spell           <- graph.from.spells(den.clean, diagonal = FALSE, minimum.duration = 1)
  }

  # Time period
  periode.start                <- period[[1]]
  periode.slut                 <- period[[2]]

  
  
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }

  r                            <- graph.spell$reference.month
  start.date                   <- elapsed_months(periode.start, r) + 1
  end.date                     <- elapsed_months(periode.slut, r)
  period.months                <- start.date : end.date
  period.names                 <- as.Date(r + months(period.months))

  # Adjacency list
  if(is.null(adjacency.list)){
  cat("\n", "Constructing the adjacency list", "\n")
  adjacency.list      <- weighted.adjacency.list(graph.spell, start = min(period.months), end = max(period.months),  m = median.term, max.terms = max.terms, boost = boost, to.distance = TRUE)
  }

  # Core identificiation
  cat("\n", "Identifying cores", "\n")
  core.list           <- llply(adjacency.list, get.shell, start.level = 1, verbose = FALSE, .progress = "text")

  # Coreness matrix
  m           <- lapply(core.list, function(x) data.frame(Name = names(x), coreness = x, row.names = NULL, stringsAsFactors = FALSE))
  names(m)    <- period.names
  m           <- bind_rows(m, .id = "Month")
  m$coreness[is.infinite(m$coreness)] <- 0
  
  coreness.mat  <- tidyr::spread(data = m, value = coreness, key = Month)
  coreness.mat  <- coreness.mat[!is.na(coreness.mat$Name), ]
  cm            <- coreness.mat[, -1]
  cm[is.na(cm)] <- 0
  rownames(cm)  <- coreness.mat$Name
  
  out                 <- list()
  
  out$spell.graph     <- graph.spell
  out$adjacency.list  <- adjacency.list 
  out$cores           <- cm
  out$period          <- period
  out
}

describe.cores <- function(cores){
    
  # Cores 
  cm                <- cores$cores
  coreness          <- sapply(cm, max, na.rm = T)
  in.core           <- t(t(cm) == coreness)
  in.core[, coreness == 0]           <- FALSE
  core.size         <- colSums(in.core)
  linker.size       <- colSums(cm > 0)
  
  # Smooth core
  running.median.size  <- (runmed(core.size, k = 11))
  collapse.sequences   <- lapply(cm, function(x) cumsum(rev(table(x))))
  for(i in 1:length(collapse.sequences)) collapse.sequences[[i]] <- collapse.sequences[[i]] - running.median.size[i]

    smooth.coreness.by.size   <- sapply(collapse.sequences, function(x){
    x <- stack(x)
    as.numeric(as.character(x$ind[x$values >= 0][1]))
    })
  
  in.smooth.core           <- t(t(cm) >= smooth.coreness.by.size)
  in.smooth.core[, coreness == 0]           <- FALSE
  
  core.size.smooth         <- colSums(in.smooth.core)
  
  
  # Adjacencies

  number.of.edges     <- sapply(cores$adjacency.list, function(x) length(x@x)) / 2 
  number.of.nodes     <- sapply(cores$adjacency.list, function(x) sum(rowSums(x) > 0))
  sum.of.edges        <- sapply(cores$adjacency.list, function(x) sum(1/x@x)) / 2 
  sum.of.strong.edges <- sapply(cores$adjacency.list, function(x) sum(1/ x@x[x@x <= 1] )) / 2 
  
  # Individuals
  va                   <- as_tibble(get.vertex.attribute(cores$spell.graph))
  va                   <- va[va$name %in% rownames(in.core),]
  va$retire.date       <- as.Date(cores$spell.graph$reference.month + months(va$retire))
  va$core.months       <- rowSums(in.core)
  va$core.smooth.months <- rowSums(in.smooth.core)
  va$linker.months     <- rowSums(in.core > 0)
  
  
  out       <- list()
  out$cores <-
    tibble(
      "Month" = colnames(cm),
      "Coreness" = coreness,
      "Core size" = core.size,
      "Coreness smooth" = smooth.coreness.by.size,
      "Core size smooth" = core.size.smooth,
      "Linker size" = linker.size,
      "Number of edges" = number.of.edges,
      "Number of nodes" = number.of.nodes,
      "Sum of edges" = sum.of.edges,
      "Sum of strong.edges" = sum.of.strong.edges
    )
  
  out$ind.results        <- va
  out$in.smooth.cores    <- in.smooth.core
  out
}