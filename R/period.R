# period ----

as.period.den <- function(den, start, end, interval = "month"){
  
  
}

summary.period.den <- function(den){
  
  
}

by.period          <- function(den){
  
}

# Create a period graph ----

#' Create an igraph object from position spells
#' 
#' Makes a graph from a den object with time spells e.g. a row looks like this;
#' NAME = Sam Samsen, AFFILIATION = Samson affiliates, PERSON_START = "2009-08-03", PERSON_END = "2009-12-02".
#' PERSON_START and PERSON_END has to be Date objects, see \link{as.Date} and \link{lubridate}.
#' 
#' @param den a den class object
#' @param diagonal if TRUE self ties, sometimes called loops, are included
#' @param minimum.duration the minimum edge duration in number of days
#' @param reference.month all dates are converted to the number of months since the reference month. This is to save costly conversions between character and date formats.
#'   
#' @return an igraph object
#' @export
#' 
#' @examples

graph.from.spells  <- function(den, diagonal = FALSE, minimum.duration = 1, reference.month = strptime("1900-01-01", format = "%Y-%m-%d")){
  
  # Checks and quality
  if (is.den(den) == FALSE) stop("den is not a den.class object")
  if (is.null(den$"PERSON_START")) stop("den has no PERSON_START column")
  if (is.null(den$"PERSON_END")) stop("den has no PERSON_END column")
  
  if (lubridate::is.Date(den$"PERSON_START") == FALSE) stop("PERSON_START is not in Date format")
  if (lubridate::is.Date(den$"PERSON_END") == FALSE) stop("PERSON_END is not in Date format")

    den              <- droplevels(den[complete.cases(den[, c("PERSON_START", "PERSON_END")]),])
    
    # You can't make ties between egos and alters in affiliations with a single member
    # Therefore we remove them
    
    ts                  <- stack(table(den$AFFILIATION))
    empty.affils        <- as.character(ts$ind)[ts == 1]
    den                 <- droplevels(den[(den$AFFILIATION %in% empty.affils) == FALSE,])
    
  
  spell.edges           <- function(x, diagonal, minimum.duration){
    x.intervals                     <- interval(x$PERSON_START, x$PERSON_END)
    o                               <- outer(x.intervals, x.intervals, lubridate::intersect)
    o[upper.tri(o, diag = diagonal)]   <- interval(NA, NA)
    dur                             <- which(o@.Data <= as.numeric(lubridate::ddays(minimum.duration)))  
    o[dur]                          <- lubridate::interval(NA, NA) 
    
    d              <- as.data.frame(split(1:length(o), ceiling(seq_along(o)/length(x.intervals))), check.names = FALSE )
    rownames(d)    <- colnames(d)
    m              <- melt(name_rows(d), id.vars = ".rownames", variable.name = "ego")
    m$alter        <- factor(as.numeric(m$.rownames))
    m$position_id  <- factor(m$.rownames)
    
    levels(m$position_id) <- x$POSITION_ID
    levels(m$ego)   <- x$NAME
    levels(m$alter) <- x$NAME
    m$start         <- lubridate::int_start(o)
    m$end           <- lubridate::int_end(o)
    
    m               <- m[complete.cases(m),]
    m[, c("ego", "alter", "start", "end", "position_id")]
  }
  
  # Spells
  den             <- group_by(.data = den, AFFILIATION)
  spells          <- do(.data = den, spell.edges(., diagonal = diagonal, minimum.duration = minimum.duration))
  
  # Edge attributes
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  spells$start        <- elapsed_months(spells$start, reference.month)
  spells$end          <- elapsed_months(spells$end, reference.month)
  spells$position_id  <- as.character(spells$position_id) 
  
  # Vertex attributes
  end.ego             <- spells %>% dplyr::group_by(., ego) %>% dplyr::summarize(end.ego = max(end))
  end.alter           <- spells %>% dplyr::group_by(., alter) %>% dplyr::summarize(end.alter = max(end))
  vm                  <- dplyr::full_join(end.ego, end.alter, c("ego" = "alter")) %>% dplyr::group_by(ego) %>% dplyr::summarize(retire = pmax(end.ego, end.alter))
  
  # Graph creation
  spells              <- spells %>% dplyr::select(ego, alter, everything())
  graph               <- graph_from_data_frame(spells, directed = FALSE, vertices = vm)
  
  # Graph attributes
  graph$reference.month <- reference.month
  
  graph
}


#' Title
#'
#' @param graph.spell 
#' @param minimum.gap 
#'
#' @return
#' @export
#'
#' @examples

prior.connections    <- function(graph.spell, minimum.gap = 12){
  
  # Remove loops, but keep multiple edges
  graph.prior          <- simplify(graph.spell, remove.multiple = FALSE, remove.loops = TRUE)
  # Count multiples and remove non-multiple
  graph.prior          <- delete.edges(graph.prior, which(count_multiple(graph.prior) == 1))
  
  find.inactive.months <- function(sequences){
    active.months        <- unique(unlist(apply(sequences, 1, function(x) seq(from = x[1], to = x[2]))))
    all.months           <- min(sequences):max(sequences)
    out                  <- all.months %in% active.months
    attributes(out)$start      <- min(active.months)
    attributes(out)$end        <- max(active.months)
    out
  }
  
  
  # Create edge list
  participants <- get.edgelist(graph.prior)
  participants <- paste(participants[, 1], participants[, 2], sep = " %--% ")
  ed           <- data_frame(start = E(graph.prior)$start, end = E(graph.prior)$end, participants = participants, id = E(graph.prior)$position_id)
  
  # Set dates according to the reference month
  
  # Her har jeg lidt hurtigt smidt de her linjer ud, men output ser ok ud.
  # reference.month <- graph.spell$reference.month
  # ed$start     <- elapsed_months(ed$start, reference.month)
  # ed$end       <- elapsed_months(ed$end, reference.month)
  # 
  # # Find the inactive months
  
  
  out          <- by(data = ed[, 1:2], INDICES = ed$participants, find.inactive.months)
  # 
  # Remove all edges without gaps or with too short gaps
  pause.length <- sapply(out, function(x) sum(x == FALSE))
  out          <- out[pause.length >= minimum.gap]
  
  # Out
  out
}

#' Title
#'
#' @param end_date 
#' @param start_date 
#'
#' @return
#' @export
#'
#' @examples
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#' Title
#'
#' @param spell.graph 
#' @param period 
#'
#' @return
#' @export
#'
#' @examples

period.graph <- function(spell.graph, start, end){
  del        <- which(E(spell.graph)$start > end | E(spell.graph)$end < start)
  g          <- delete.edges(spell.graph, del)
  simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "ignore")
}


#' Title
#'
#' @param spell.graph 
#' @param start 
#' @param end 
#' @param to.distance 
#' @param distance.weight 
#' @param decay 
#'
#' @return
#' @export
#'
#' @examples
weighted.graph <- function(spell.graph, start, end, to.distance = TRUE, distance.weight = distance.weight, decay = decay, m = 38){
  
  start.boost <- function(x, boost = 12){
    x[x <= boost] <- boost
    x
  }
  
  del          <- which(E(spell.graph)$start > end | E(spell.graph)$end < start)
  g            <- delete.edges(spell.graph, del)
  
  period       <- start:end
  adj.cum      <- get.adjacency(period.graph(g, start = period[1], end = period[1]))
  pb           <- txtProgressBar(min = 2, max = length(period), style = 3)
  for (i in 2:length(period)) {
    t            <- period[i]
    adj.t        <- get.adjacency(period.graph(g, start = t, end = t))
    adj.cum      <- adj.cum + adj.t
    sv.cum       <- as(adj.cum, Class = "sparseVector")
    sv.t         <- as(adj.t, Class = "sparseVector")
    set          <- sv.cum@i %in% sv.t@i # The active set of ties
    sv.cum@x[!set]   <- decay(sv.cum@x[!set])
    sv.cum@x[set]    <- start.boost(sv.cum@x[set], boost = 12)
    adj.cum@x        <- sv.cum@x
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (identical(to.distance, TRUE))  adj.cum@x <- distance.weight(adj.cum@x) 
  
  graph         <- graph_from_adjacency_matrix(adj.cum, mode = "undirected", weighted = TRUE)

  # Remove the retired
  retired                 <- V(spell.graph)$retire < end
  if (any(retired)) graph <-  delete.edges(graph, E(graph)[.inc(which(retired))])
  
  graph
}

#' Title
#'
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
# decay <- function(value, max.months = 96){
#   
#   value[value >= max.months] <- max.months - 0.1
#   
#   xfun <- function(y, L, k) {(y - 60) - log(L / y - 1) / k}
#   b    <- xfun(value, 180, -0.05)
#   
#   yfun <- function(x, L, k, x0) {L / (1 + exp(-k * (x - x0)))}
#   yfun(b + 1, 180, -0.05, value - 60)
# }

decay <- function(x, m = 38, max.terms = 2){
   x[x > m * 2]       <- m * max.terms
  b  <-  m / 2       # m is the median, b is the lowest point
  t  <-  m * 2     # t is twice the median and therefore the top # not after max.terms
  decay.rate       <- (b/t)^(1/b) # The rate by which t decays in order to reach b after b months
  x * decay.rate
}



#' Title
#'
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
# distance.weight2 <- function(value, max.cut = 0.75){
#   x <- log(value, base = 12)
#   x[x < 0]     <- 0
#   x            <- 1/x
#   x[x == Inf]  <- 0
#   x[x <= max.cut] <- max.cut
#   x
#}

distance.weight <- function(x, m = 38, max.terms = 2){
  x[x > m*max.terms]    <- m * max.terms
  1/(x/(m))
}


#' Title
#'
#' @param x 
#' @param boost 
#' @param max 
#'
#' @return
#' @export
#'
#' @examples

accumulation.thresholds   <- function(x, boost = 12, max = 76){
  x[x < boost] <- boost
  x[x > max]   <- max
  x
}

  
#' Create a list af weighted adjacency matrices
#' 
#' A more efficient way of creating time series of \link{weighted.graph}
#'
#' @param spell.graph a spell.graph
#' @param start 
#' @param end 
#' @param to.distance if TRUE weights are recalculated with \link{distance.weight}, otherwise weighted months
#'
#' @return
#' @export
#'
#' @examples

weighted.adjacency.list <- function(spell.graph, start, end, m = 38, boost = m, max.terms = 3, to.distance = TRUE){
  
  del          <- which(E(spell.graph)$start > end | E(spell.graph)$end < start)
  g            <- delete.edges(spell.graph, del)
  
  period       <- start:end
  adj.cum      <- get.adjacency(period.graph(g, start = period[1], end = period[1]))
  pb           <- txtProgressBar(min = 2, max = length(period), style = 3)
  
  adj.list       <- list()
  adj.list[[1]]  <- adj.cum
  
  for (i in 2:length(period)) {
    t            <- period[i]
    gp           <- period.graph(g, start = t, end = t)
    adj.t        <- get.adjacency(gp)
    adj.cum      <- adj.cum + adj.t
    
    # Remove the retired
    retired      <- V(gp)$retire == (t - 1) # Did they retire the previous month?
    adj.cum[retired, ] <- adj.cum[retired,] * 0
    adj.cum[, retired] <- adj.cum[, retired] * 0
    adj.cum      <- drop0(adj.cum, tol = 0.1) # Here we reduce the data-size - and drop all weak ties
    
    sv.cum       <- as(adj.cum, Class = "sparseVector")
    sv.t         <- as(adj.t, Class = "sparseVector")
    set          <- sv.cum@i %in% sv.t@i
    
    sv.cum@x[!set]   <- decay(sv.cum@x[!set], m = m, max.terms = max.terms)
    adj.cum@x        <- sv.cum@x
    adj.list[[i]]    <- adj.cum
    
    adj.list[[i]]@x[set] <- accumulation.thresholds(sv.cum@x[set], boost = boost, max = m * max.terms)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (identical(to.distance, TRUE))  adj.list <- llply(adj.list, function(x){ x@x <- distance.weight(x@x, m = m, max.terms = max.terms)
                                                                         x})
  adj.list
}


#' Turn a spell.graph into a dataset suitable for survival analysis
#'
#' Note that this code is slow.
#' 
#' @param spell.graph 
#' @param end.month an integer for the end month. The end month is the end point for all non-reoccuring ties that are broken.
#'
#' @return a data.frame
#' @export
#'
#' @examples

from.spell.graph.to.survival <- function(spell.graph, end.month = 1392){
  
  # reference.month <- spell.graph$reference.month
  # reference.month + months(1392)
  # reference.month + months(1100)
  
  gs          <- delete.edges(spell.graph, edges = which(E(spell.graph)$end >= end.month))
  gs          <- simplify(gs, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Data for the ties without reemergence -----
  gs.no.reemergence   <- delete.edges(gs, which(count_multiple(gs) > 1))
  
  data.no.reemergence <- data.frame(remergence     = FALSE,
                                    duration       = E(gs.no.reemergence)$end - E(gs.no.reemergence)$start,
                                    break.duration = end.month - E(gs.no.reemergence)$end,
                                    start.month    = E(gs.no.reemergence)$start
  )
  
  gender.a <- code.gender(firstnames(head_of(gs.no.reemergence, E(gs.no.reemergence))$name))
  gender.b <- code.gender(firstnames(tail_of(gs.no.reemergence, E(gs.no.reemergence))$name))
  
  data.no.reemergence$gender.similarity <- gender.a == gender.b
  
  # Data for the ties that reemerge ----
  prior        <- prior.connections(gs, minimum.gap = 0)
  prior.rle    <- lapply(prior, function(x) rle(as.vector(x)))
  prior.start  <- sapply(prior, function(x) attributes(x)$start)
  prior.end    <- sapply(prior, function(x) attributes(x)$end)
  
  prior.dat   <- list()
  
  pb           <- txtProgressBar(min = 1, max = length(prior.rle), style = 3)
  for (i in 1:length(prior.rle)) {
    
    # Add the length of the final break
    l                <- length(prior.rle[[i]]$lengths)
    x                <- prior.rle[[i]]
    x$lengths[l + 1] <- end.month - prior.end[i]
    x$values[l + 1]  <- FALSE 
    
    # Gender
    n                <- names(prior.rle)[i]
    n                <- unlist(str_split(n, pattern = " %--% ", n = 2))
    n                <- firstnames(n)
    n                <- as.character(code.gender(n))
    n[is.na(n)]      <- "FALSE"
    n                <- n[1] == n[2]
    
    # Collect
    out       <- data.frame(remergence     = TRUE,
                            duration       = x$lengths[x$values],
                            break.duration = x$lengths[x$values == FALSE],
                            start.month    = as.numeric(prior.start[i]),
                            gender.similarity = n)  
    # The final break does not remerge
    out$remergence[nrow(out)]   <- FALSE
    
    prior.dat[[i]] <- out
    setTxtProgressBar(pb = pb, value = i)
  } 
  
  data.reemergence <- bind_rows(prior.dat)
  
  data.all         <- rbind(data.reemergence, data.no.reemergence)
  
  data.all$remergence <- as.numeric(data.all$remergence)
  data.all$gender.similarity <- as.numeric(data.all$gender.similarity)
  
  data.all
}

