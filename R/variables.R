# Functions that create variables on the basis of den objects


name.id <- function(name, id){
  
  
  
}

#' A vector of members in an affiliation
#'
#' @param x a character vector with names of affiliations
#' @param den a den class object with the affiliations in x
#' @return a vector with names of the members seperated by " * "
#' @export
#' @examples 
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' graph.culture <- elite.network(den.culture, result = "affil")
#' members.vector(V(graph.culture)$name, den)

members.vector  <- function(x, den){
  den.x         <- droplevels(den[which(den$AFFILIATION %in% x),])
  l.medlem      <- lapply(x, function(x, den.x) as.character(den.x$NAME)[den.x$AFFILIATION %in% x], den.x)
  paste.names   <- unlist(lapply(l.medlem, paste, collapse = " * "))
  paste.names
}

#' A vector of memberships for each individual
#'
#' @param x a character vector with names of individuals
#' @param den 
#'
#' @return a vector
#' @export
#' @examples 
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' graph.culture <- elite.network(den.culture, result = "ind")
#' membership.vector(V(graph.culture)$name, den)

membership.vector <- function(x, den){
  den.x           <- droplevels(den[which(den$NAME %in% x),])
  l.medlemskab    <- lapply(x, function(x, den.x) unique(as.character(den.x$AFFILIATION)[den.x$NAME %in% x]), den.x)
  paste.names     <- unlist(lapply(l.medlemskab, paste, collapse = " * "))
  paste.names
}

#' A vector of descriptions for each individual
#'
#' @param x a character vector with names of individuals
#' @param den a den class object with a DESCRIPTION variable, see \link{as.den}
#'
#' @return a vector with descriptions where affiliations are seperated by "*"
#' @export
#' data(den)
#' den.culture   <- has.tags(den, "Culture", result = "den")
#' graph.culture <- elite.network(den.culture, result = "ind")
#' description.vector(V(graph.culture)$name, den)[1]

description.vector      <- function(x, den){
  den.x                 <- droplevels(den[which(den$NAME %in% x),])
  beskriv               <- function(x, den.x) {
    dat                 <- data.frame(affil = den.x$AFFILIATION, description = den.x$DESCRIPTION)[den.x$NAME %in% x,]
    dat                 <- dat[duplicated(dat$affil) == FALSE,]
    paste(dat$affil, ":",  dat$description, collapse = " * ")
  }
  sapply(x, beskriv, den.x = den.x)
}


#' Create a vector of tags on the basis of affiliation
#'
#' @param x a character vector of affiliation names
#' @param den 
#'
#' @return a vector of tags
#' @export

tag.vector    <- function(x, den){
  den.x       <- droplevels(den[which(den$AFFILIATION %in% x),])
  den.x       <- droplevels(den.x[duplicated(den.x$AFFILIATION) == FALSE,])
  den.x       <- den.x[match(den.x$AFFILIATION, x),]
  as.character(den.x$TAGS)
}

neighbors     <- function(x, graph){
  
  
  
}


# Data coding and recoding -----

#' Search descriptions
#' 
#' Find specifik search terms in all descriptions
#' @param rel a affiliation edgelist
#' @param soegeord a character vector of search terms
#' @param ignore.case if TRUE the search is not case sensitive
#' @param ... further arguments are passed on to \link{grep}.
#' @return a affiliation edgelist
#' @export

find.beskrivelse <- function(rel, soegeord, ignore.case=TRUE, ...){
  
  beskrivelse <- as.character(rel$DESCRIPTION)
  if (ignore.case == TRUE) beskrivelse <- tolower(beskrivelse)
  
  grep.soeg  <- paste(soegeord, collapse = "|")
  grep.fund <- grep(grep.soeg, beskrivelse, ignore.case = ignore.case, ...)  
  # grep.fund <- grep(grep.soeg, beskrivelse, ignore.case=ignore.case)  
  navne.fund <- levels(as.factor(rel$NAME[grep.fund]))
  navne.ind  <- which(rel$NAME %in% navne.fund)
  
  droplevels(rel[navne.ind,])
}

#' Code gender by firstname in Denmark
#' 
#' Guesses the gender for a list of names by comparing it to the Danish national distribution of first names in 2013.
#' @param x a character vector of full names
#' @return a factor with a gender guess
#' @export
#' @examples 
#' data(den)
#' priest.names   <- has.tags(den, "Churches", result = "name")
#' table(code.gender(priest.names))

code.gender <- function(x){
  names.gender    <- eliter:::names.gender[, c("Navn", "Andel.mænd")]
  Encoding(names.gender$Navn) <- "UTF-8" 
  first.x         <- firstnames(x)
  first.x         <- data.frame(Navn = I(toupper(first.x)))
  gender          <- dplyr::left_join(first.x, names.gender, by = "Navn")
  b               <- c(0, 0.2, 0.8, 1)
  out             <- cut(gender$"Andel.mænd", b, include.lowest = TRUE, labels = c("Women", "Binominal", "Men"))
  out
}

#' Extract first names
#' 
#' Extract first names from full names
#' @param x a character vector of full nmaes
#' @return a character vector of first names
#' @export
#' @examples 
#' data(den)
#' first          <- table(firstnames(den$NAME))
#' head(sort(first, decreasing = TRUE), 20)

firstnames        <- function(x){
  x               <- as.character(x)
  n.list          <- strsplit(x, " ")
  first           <- sapply(n.list, head, 1)
  first
}

#' Extract last names
#' 
#' Extract last names from full names
#' @param x a character vector of full nmaes
#' @return a character vector of last names
#' @export
#' @examples 
#' data(den)
#' last          <- table(lastnames(den$NAME))
#' head(sort(last, decreasing = TRUE), 20)

lastnames         <- function(x){
  x               <- as.character(x)
  n.list          <- strsplit(x, " ")
  x               <- sapply(n.list, tail, 1)
}

#' Categories from postal codes
#' 
#' @param x a numeric vector with 4 digit danish postal codes
#' @return a data.frame with various factors
#' @export
#' @examples 
#' data(corp13)
#' post      <- extract.postal.codes(corp13$Adress) 
#' regions   <- code.region(post)
#' table(regions$Type.of.Area)

code.region <- function(x){
  postnumre <- eliter:::postnumre[duplicated(eliter:::postnumre$POSTNR) == FALSE,]
  jx        <- data.frame(POSTNR = x)
  xm        <- dplyr::left_join(jx, postnumre, by = "POSTNR")
  xm
}


#' Extract Danish postal codes from an adress string
#'
#' @param x a character vector with addresses
#' @param check.valid if TRUE all invalid postal codes are set as NA. This affects postal codes for Greenland (3900:3992)
#' @return a numeric vector
#' @export
#'
#' @examples
#' data(corp13)
#' address            <- corp13$Adress
#' plot(table(extract.postal.codes(address)))

extract.postal.codes  <- function(x, check.valid = FALSE){
  x                   <- as.character(x)
  x                   <- str_extract(x, "[[:digit:]]{4}")
  post                <- unique(eliter:::postnumre$POSTNR)
  if (identical(check.valid, TRUE)) x[(x %in% post) == FALSE]   <- NA
  x
}


#' Geoggraphical neighbours graph
#'
#' How many people do you live close to within a given distance in meters
#'
#' @param lon a numeric vector with longitudes
#' @param lat a numeric vector with latitudes
#' @param id a character vector with names
#' @param distance maximum distance in meters
#'
#' @return a weighted graph with edge weights equal to the geographical distance
#' @export
#'
#' @examples
#' data(pe13)
#' graph.2km       <- geographical.neighbours(lon = pe13$lon, lat = pe13$lat, id = pe13$Name, distance = 2000)
#' graph.10km       <- geographical.neighbours(lon = pe13$lon, lat = pe13$lat, id = pe13$Name, distance = 10000)
#' 
#' graph.2km     <- graph.adjacency(afstand.2km, mode = "undirected", weighted = TRUE)
#' graph.10km    <- graph.adjacency(afstand.10km, mode = "undirected", weighted = TRUE)
#' 
#' deg.afstand.2km  <- degree(graph.2km)
#' deg.afstand.10km <- degree(graph.10km)
#' 
#' social.geography <- deg.afstand.2km
#' social.geography[deg.afstand.2km == 0 & deg.afstand.10km == 0] <- "Isolated at 10km"
#' social.geography[deg.afstand.2km == 0 & deg.afstand.10km != 0] <- "Isolated at 2km"
#' social.geography[deg.afstand.2km %in% 1:2]                     <- "1-2"
#' social.geography[deg.afstand.2km %in% 3:6]                     <- "3-6"
#' social.geography[deg.afstand.2km %in% 7:20]                    <- "7-20"
#' social.geography[deg.afstand.2km %in% 21:30]                   <- "21-30"
#' social.geography[deg.afstand.2km %in% 31:max(deg.afstand.2km)] <- "+30"
#' as.matrix(table(social.geography))


geographical.neighbours  <- function(lon, lat, id = 1:length(lon), distance = 500){
  x                         <- cbind(lon, lat)
  geodist                   <- distm(x, x)
  dimnames(geodist)         <- list(id, id)
  
  geodist[is.na(geodist)]     <- 0
  geodist[geodist > distance] <- 0
  
  graph.adjacency(geodist, mode = "undirected", weighted = TRUE)
}

