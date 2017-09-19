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
  
  # Graph creation
  
  den             <- group_by(.data = den, AFFILIATION)
  spells          <- do(.data = den, spell.edges(., diagonal = diagonal, minimum.duration = minimum.duration))
  el              <- as.matrix(spells[, c("ego", "alter")])
  graph           <- graph_from_edgelist(el, directed = FALSE)
  
  # Reference month
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  spells$start     <- elapsed_months(spells$start, reference.month)
  spells$end       <- elapsed_months(spells$end, reference.month)
  
  # Edge attributes
  E(graph)$start <- spells$start
  E(graph)$end   <- spells$end
  E(graph)$position_id   <- as.character(spells$position_id)
  
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
    inactive.months      <- setdiff(all.months, active.months)
    inactive.months
  }
  
  # Create edge list
  participants <- get.edgelist(graph.prior)
  participants <- paste(participants[, 1], participants[, 2], sep = " %--% ")
  ed           <- data_frame(start = E(graph.prior)$start, end = E(graph.prior)$end, participants = participants, id = E(graph.prior)$position_id)
  
  # Set dates according to the reference month
  
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  # Her har jeg lidt hurtigt smidt de her linjer ud, men output ser ok ud.
  # reference.month <- graph.spell$reference.month
  # ed$start     <- elapsed_months(ed$start, reference.month)
  # ed$end       <- elapsed_months(ed$end, reference.month)
  # 
  # # Find the inactive months
  out          <- by(data = ed[, 1:2], INDICES = ed$participants, find.inactive.months)
  # 
  # Remove all edges without gaps or with too short gaps
  pause.length <- sapply(out, length)
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
weighted.graph <- function(spell.graph, start, end, to.distance = TRUE, distance.weight = distance.weight, decay = decay){
  
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
    set          <- sv.cum@i %in% sv.t@i
    sv.cum@x[!set]   <- decay(sv.cum@x[!set])
    adj.cum@x        <- sv.cum@x
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if(identical(to.distance, TRUE))  adj.cum@x <- distance.weight(adj.cum@x) 
  
  graph_from_adjacency_matrix(adj.cum, mode = "undirected", weighted = TRUE)
}

#' Title
#'
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
decay <- function(value, max.months = 60){
  
  value[value >= max.months] <- max.months - 0.1
  
  xfun <- function(y, L, k) {(y - 60) - log(L / y - 1) / k}
  b    <- xfun(value, 180, -0.05)
  
  yfun <- function(x, L, k, x0) {L / (1 + exp(-k * (x - x0)))}
  yfun(b + 1, 180, -0.05, value - 60)
}

#' Title
#'
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
distance.weight <- function(value, max.cut = 0.75){
  x <- log(value, base = 12)
  x[x < 0]     <- 0
  x            <- 1/x
  x[x == Inf]  <- 0
  x[x <= max.cut] <- max.cut
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

weighted.adjacency.list <- function(spell.graph, start, end, to.distance = TRUE){
  
  del          <- which(E(spell.graph)$start > end | E(spell.graph)$end < start)
  g            <- delete.edges(spell.graph, del)
  
  period       <- start:end
  adj.cum      <- get.adjacency(period.graph(g, start = period[1], end = period[1]))
  pb           <- txtProgressBar(min = 2, max = length(period), style = 3)
  
  adj.list       <- list()
  adj.list[[1]]  <- adj.cum
  
  for (i in 2:length(period)) {
    t            <- period[i]
    adj.t        <- get.adjacency(period.graph(g, start = t, end = t))
    adj.cum      <- adj.cum + adj.t
    sv.cum       <- as(adj.cum, Class = "sparseVector")
    sv.t         <- as(adj.t, Class = "sparseVector")
    set          <- sv.cum@i %in% sv.t@i
    sv.cum@x[!set]   <- decay(sv.cum@x[!set])
    adj.cum@x        <- sv.cum@x
    adj.list[[i]]    <- adj.cum
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (identical(to.distance, TRUE))  adj.list <- llply(adj.list, function(x){ x@x <- distance.weight(x@x)
                                                                         x})
  adj.list
}
