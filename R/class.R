# den.class ------

#' Convert data.frame to the den format.
#'
#' If you have a data.frame with case - affiliation edges: where each row is a position in a affiliation, it can be converted to den class object.
#' The required column names are "NAME" for the names of the individuals and "AFFILIATION" for the names of the affiliations.
#' Common variables include: ROLE, TAGS, PERSON_ID, DESCRIPTION, GENDER
#'
#' @param x a data.frame
#'
#' @return a den-class object
#' @export
#'
#' @examples
#' data(den)
#' as.den(den)
as.den <- function(x){

  # What are the obligatory variables? NAME, AFFILIATION, TAGS, ROLE
  x                                             <- as.data.frame(x)

  # Do we have the obligatory variables?
  ob.var       <- c("NAME", "AFFILIATION")
  if (all(ob.var %in% colnames(x)) == FALSE) stop("Either NAME or AFFILIATION is missing")

  # Make sure there is a ROLE variable
  if (is.null(x$ROLE)) x$ROLE                    <- NA

  # Make sure there is a TAGS variable
  if (is.null(x$TAGS)) x$TAGS                    <- NA

  # Make sure there is a POSITION_ID
  if (is.null(x$POSITION_ID)) x$POSITION_ID      <- 1:nrow(x)

  # Reorder columns
  a                                              <- c("NAME", "AFFILIATION", "ROLE", "TAGS", "POSITION_ID")
  col.order                                      <- c(a, setdiff(colnames(x), a))
  x                                              <- x[, col.order]

  # Name and affiliations as character
  x$NAME              <- as.character(x$NAME)
  x$AFFILIATION       <- as.character(x$AFFILIATION)

  # Assign class
  class(x) <- c("den", "data.frame")

  x
}

#' Is this object a den object?
#'
#' Check if it is a den class object
#'
#' @param x an R object
#'
#' @return
#' @export

is.den <- function(x){
  inherits(x, "den")
}

#' Print den objects
#'
#' Essential statistics for the den-class. Mainly for interactive use, but is returned in rmarkdown format using kable and knitr for easy export.
#'
#' @param x a den-class object
#' @param ... further arguments are ignored
#' @return a markdown table with statistics
#' @method print den
#' @export
#' @examples 
#' data(den)
#' as.den(den)

print.den      <- function(x, ...){
  den          <- x 
  n.name       <- length(unique(den$NAME))
  n.rows       <- nrow(den)
  n.affil      <- length(unique(den$AFFILIATION))
  exclude      <- c("NAME", "AFFILIATION")
  vars         <- colnames(den)
  vars         <- vars[-which(colnames(den) %in% exclude)]
  var.na       <- sapply(den, function(x) sum(is.na(x))/length(x))
  t.name       <- table(den$NAME)
  n.linkers    <- sum(t.name > 1)

  stats       <- c("Names"          = n.name,
                   "Positions"      = n.rows,
                   "Affiliations"   = n.affil,
                   "Linkers"        = n.linkers,
                   "Variables"      = ncol(den))
  print.vars   <- head(vars, length(stats))
  var.na       <- head(var.na[-which(colnames(den) %in% exclude)], length(stats))
  var.na       <- round(var.na, 2)

  # Random edges
  samp         <- sample(1:nrow(den), length(stats))
  den.samp     <- den[samp, c("NAME", "AFFILIATION")]
  den.samp     <- sapply(den.samp, as.character)
  den.samp     <- apply(den.samp, 2, strtrim, 40)

  end         <-  data.frame("NAME" = format(den.samp[, 1]),
                             "AFFILIATION" = format(den.samp[, 2]))

  # Top tags
  tag.test              <- is.null(den$TAGS) == FALSE
  if (tag.test) tag.test <- all(is.na(den$TAGS)) == FALSE
  
  if (tag.test) {
  tags         <- show.all.tags(den)
  tags         <- tags[order(tags[,2], decreasing = T), ]
  end          <- data.frame("Top tags" = rownames(tags),
                             "Affiliations" = format(tags[, 2], big.mark = ","))
  end          <- head(end, length(stats))
  }

out            <- data.frame("Stats" = format(stats, big.mark = ",", justify = "right"),
                             " " = " ",

                             "Variables" = format(print.vars),
                             "NA (%)" = format(var.na),
                             " " = " ",


                             check.names = FALSE)
out            <- cbind(out, end)

print(kable(out, align = c("r", "c", "l", "r", "l", "l", "r")))
}

#' Summary statistics for den
#'
#' @param x a den class object
#'
#' @return a list of markdown tables
#' @export
#' 
#'
#' @examples
#' 
summary.den         <- function(x, ...){
  incidence         <- xtabs(~NAME + AFFILIATION, x, sparse = T) 
  adj.ind           <- incidence %*% Matrix::t(incidence)
  diag(adj.ind)     <- 0
  
  adj.affil         <- Matrix::t(incidence) %*% incidence
  diag(adj.affil)   <- 0
  
  out.list          <- list()
  
  # Descriptives for Individuals: Memberships (IQT), degree (IQT), positions held by linkers n og %, isolated individuals n og %, multiple ties n og %
  memberships       <- Matrix::rowSums(incidence)
  memberships.iqr   <- quantile(memberships)
  degree.ind        <- Matrix::rowSums(adj.ind > 0)
  degree.ind.iqt    <- quantile(degree.ind)

  pos.by.linkers          <- format(sum(memberships[memberships > 1]), big.mark = ",")
  pos.by.linkers[2]       <- percent(sum(memberships[memberships > 1]) / sum(memberships))
  
  
  multiple.ties           <- format(sum(adj.ind@x > 1) / 2, big.mark = ",")
  multiple.ties[2]        <- percent((sum(adj.ind@x > 1) / 2) / (sum(adj.ind@x > 0) / 2))
  
  iqts                <- data.frame("Memberships" = memberships.iqr,
                                    "Degree"      = degree.ind.iqt,
                                  check.names   = FALSE)
  iqts                <- t(iqts)
  
  ns                  <- data.frame("Positions held by linkers" = pos.by.linkers,
                                    "Multiple ties"             = multiple.ties,
                                    check.names = FALSE)
  ns                  <- cbind(".", ".", ".", t(ns))
  
  filler              <- c(".", ".", ".", "N.", "%")
  
  
  left                <- (rbind(iqts, filler , ns))
  
  top.mem             <- head(sort(memberships, decreasing = TRUE), nrow(left))
  right               <- data.frame("Most memberships" = names(top.mem), "Memberships" = top.mem, row.names = NULL)
  
  out                 <- cbind(left, " " = "" , right)
  out.list$individuals       <- kable(out)
  
  
  # Descriptives for affiliations: Members, degree, isolated affiliations, 
  members             <- Matrix::colSums(incidence)
  members.iqr         <- quantile(members)
  
  degree.affil        <- Matrix::rowSums(adj.affil > 0)  
  degree.affil.iqr    <- quantile(degree.affil)
  
  isolates            <- format(sum(degree.affil == 0), big.mark = ",")
  isolates[2]         <- percent(sum(sum(degree.affil == 0) / length(degree.affil)))
  
  affil.ties          <- format(sum(adj.affil@x > 1) / 2, big.mark = ",")
  affil.ties[2]       <- percent((sum(adj.affil@x > 1) / 2) / (sum(adj.affil@x > 0) / 2))
  
  iqrs                <- data.frame("Members" = members.iqr,
                                    "Degree"  = degree.affil.iqr,
                                    check.names = FALSE)
  
  iqrs                <- format(t(iqrs), big.mark = ",")
  
  ns                  <- data.frame("Isolates"                  = isolates,
                                    "Multiple ties"             = affil.ties,
                                    check.names = FALSE)
  ns                  <- cbind(".", ".", ".", t(ns))
  
  filler              <- c(".", ".", ".", "N.", "%")
  
  
  left                <- (rbind(iqts, " " = filler , ns))
  
  top.mem             <- head(sort(members, decreasing = TRUE), nrow(left))
  right               <- data.frame("Largest affiliations" = names(top.mem), "Members" = top.mem, row.names = NULL, check.names = FALSE)
  right               <- format(right, big.mark = ",")
  
  out                         <- cbind(left, " " = "" , right)
  out.list$affiliations       <- kable(out)
  
    
  # Descriptives for tags: Most common tags, top tag combinations, 
  tag.test              <- is.null(x$TAGS) == FALSE
  if (tag.test) tag.test <- all(is.na(x$TAGS)) == FALSE
  
  if (tag.test) {
  x.unique            <- x[duplicated(x$AFFILIATION) == FALSE,]
  split.den           <- split(x = x.unique, f = x.unique$AFFILIATION)
  
  affil.to.tag.net     <- function(x){
    tag.vector         <- unlist(strsplit(as.character(x$TAGS), ", "))
    affiliation.vector <- rep(as.character(x$AFFILIATION), times = length(tag.vector))
    cbind(AFFILIATION = affiliation.vector , TAG = tag.vector)
  }
  
  split.tag.edges     <- lapply(split.den, affil.to.tag.net)
  tag.edges           <- do.call(rbind, split.tag.edges)
  tag.incidence       <- xtabs(~ AFFILIATION + TAG, tag.edges, sparse = TRUE)
  adj.tag             <- Matrix::t(tag.incidence) %*% tag.incidence
  diag(adj.tag)       <- 0
  
  net.tag             <- graph.adjacency(adj.tag, weighted = TRUE, mode = "undirected")
  edges.tag           <- data.frame("Edges" = get.edgelist(net.tag), "Weight" = E(net.tag)$weight)
  edges.tag           <- edges.tag[order(edges.tag$Weight, decreasing = TRUE),]
  edges.tag           <- data.frame("Co-tag" = paste0(edges.tag[, 1], " - ", edges.tag[, 2]), "Affils" = edges.tag$Weight, check.names = FALSE, row.names = NULL)
  
  # Number of tags iqr
  n.tags.iqr         <- quantile(Matrix::rowSums(tag.incidence))
  
  # Number of affiliations per tag iqr
  n.affils.per.tag  <- quantile(Matrix::colSums(tag.incidence))
  
  iqts              <- data.frame("Tags per affil." = n.tags.iqr,
                                  "Affils per tag"  = n.affils.per.tag,
                                  check.names = FALSE)
  iqts             <- format(t(iqts), big.mark = ",")
  
  # Number of events
  events           <- sum(tag.edges[, 2] == "Events")
  events[2]        <- percent(round(sum(tag.edges[, 2] == "Events") / nrow(x.unique), 2))

  # Untagged       
  untagged         <- length(unique(den$AFFILIATION)) - nrow(tag.incidence)
  untagged[2]      <- percent(round(untagged / nrow(tag.incidence), 2))
  
  
  # out tags  
  ns                  <- data.frame("Events"                    = events,
                                    "Untagged"                  = untagged,
                                    check.names = FALSE)
  ns                  <- cbind(".", ".", ".", t(ns))
  
  filler              <- c(".", ".", ".", "N.", "%")
  
  
  left                <- (rbind(iqts, " " = filler , ns))
  
  right               <- head(edges.tag, nrow(left))
  
  right               <- format(right, big.mark = ",")
  
  out                 <- cbind(left, " " = "" , right)
  out.list$tags       <- kable(out)
  }
  
  # Out
  
  out.list
  
  
}

#' @method plot den
#' @export

plot.den <- function(x, ...){
  # Facets with: Number of memberships, number of members, graph strength for individuals and affiliations
  incidence     <- xtabs(~ NAME + AFFILIATION, data = x, sparse = TRUE)
  members       <- Matrix::colSums(incidence)
  memberships   <- Matrix::rowSums(incidence)
  
  t.memberships   <- table(memberships)
  t.members       <- table(members)
  
  members         <- data.frame(value = sort(members, decreasing = TRUE), id = 1:length(members), var = "Members", row.names = NULL)
  memberships     <- data.frame(value = sort(memberships, decreasing = TRUE), id = 1:length(memberships), var = "Memberships", row.names = NULL)
  t.members       <- data.frame(value = as.numeric(t.members), id = names(t.members), var = "Frequency: Members", row.names = NULL)
  t.memberships   <- data.frame(value = as.numeric(t.memberships), id = names(t.memberships), var = "Frequency: Memberships", row.names = NULL)
  
  t.members       <- t.members[cumsum(t.members$value) / sum(t.members$value) <= 0.95,]
  
  md              <- rbind(members, memberships, t.members, t.memberships, rownames = NULL)
  
  p     <- ggplot(md, aes(x = as.numeric(id), y = value, group = var)) + facet_wrap(~var, scales = "free")
  p     <- p + geom_line(size = 0.5) + geom_point(size = 0.5) + ggthemes::theme_tufte() + ggthemes::geom_rangeframe()
  p     <- p + xlab("") + ylab("")
  print(p)
}