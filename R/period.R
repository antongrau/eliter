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
#'   
#' @return an igraph object
#' @export
#' 
#' @examples

graph.from.spells  <- function(den, diagonal = FALSE, minimum.duration = 1){
  
  # Checks and quality
  if (is.den(den) == FALSE) stop("den is not a den.class object")
  if (is.null(den$"PERSON_START")) stop("den has no PERSON_START column")
  if (is.null(den$"PERSON_END")) stop("den has no PERSON_END column")
  
  if (lubridate:::is.Date(den$"PERSON_START") == FALSE) stop("PERSON_START is not in Date format")
  if (lubridate:::is.Date(den$"PERSON_END") == FALSE) stop("PERSON_END is not in Date format")
  den              <- droplevels(den[complete.cases(den[, c("PERSON_START", "PERSON_END")]),])
  
  spell.edges           <- function(x, diagonal, minimum.duration){
    x.intervals                     <- interval(x$PERSON_START, x$PERSON_END)
    o                               <- outer(x.intervals, x.intervals, intersect)
    o[upper.tri(o, diag = diagonal)]   <- interval(NA, NA)
    dur                             <- which(o@.Data <= as.numeric(ddays(minimum.duration)))  
    o[dur]                          <- interval(NA, NA) 
    
    d              <- as.data.frame( split(1:length(o), ceiling(seq_along(o)/length(x.intervals))) )
    rownames(d)    <- colnames(d)
    m              <- melt(name_rows(d), id.vars = ".rownames", variable.name = "ego")
    m$alter        <- factor(m$.rownames)
    m$position_id  <- factor(m$.rownames)
    
    levels(m$position_id) <- x$POSITION_ID
    levels(m$ego)   <- x$NAME
    levels(m$alter) <- x$NAME
    m$start         <- int_start(o)
    m$end           <- int_end(o)
    
    m               <- m[complete.cases(m),]
    m[, c("ego", "alter", "start", "end", "position_id")]
  }
  
  # Graph creation
  
  den             <- group_by(.data = den, AFFILIATION)
  spells          <- do(.data = den, spell.edges(., diagonal = diagonal, minimum.duration = minimum.duration))
  el              <- as.matrix(spells[, c("ego", "alter")])
  graph           <- graph_from_edgelist(el, directed = FALSE)
  
  # Edge attributes 
  
  E(graph)$start <- as.character(spells$start)
  E(graph)$end   <- as.character(spells$end)
  E(graph)$position_id   <- as.character(spells$position_id)
  graph
}