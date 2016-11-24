# Functions that create variables on the basis of den objects


name.id <- function(name, id){
  
  
  
}

#' A vector of members in an affiliation
#'
#' @param x a character vector with names of affiliations
#' @param den 
#' @return a vector
#' @export
member.vector <- function(x, den){
  den.x       <- droplevels(den[which(den$AFFILIATION %in% x),])
  l.medlem    <- lapply(x, function(x, den.x) as.character(den.x$NAME)[den.x$AFFILIATION %in% x], den.x)
  paste.names <- unlist(lapply(l.medlem, paste, collapse = " * "))
  paste.names
}

#' A vector of memberships for each individual
#'
#' @param x a character vector with names of individuals
#' @param den 
#'
#' @return a vector
#' @export

membership.vector <- function(x, den){
  den.x           <- droplevels(den[which(den$NAME %in% x),])
  l.medlemskab    <- lapply(x, function(x, den.x) unique(as.character(den.x$AFFILIATION)[den.x$NAME %in% x]), den.x)
  paste.names     <- unlist(lapply(l.medlemskab, paste, collapse = " * "))
  paste.names
}

#' A vector of descriptions for each individual
#'
#' @param x a character vector with names of individuals
#' @param den 
#'
#' @return a vector
#' @export

description.vector <- function(x, den){
  den.x           <- droplevels(den[which(den$NAME %in% x),])
  beskriv         <- function(x, den.x) {
    dat             <- data.frame(affil = den.x$AFFILIATION, description = den.x$DESCRIPTION)[den.x$NAME %in% x,]
    dat             <- dat[duplicated(dat$affil) == FALSE,]
    paste(dat$affil, ":",  dat$description, collapse = " * ")
  }
  sapply(x, beskriv, den.x = den.x)
}


#' Create a vector of tags on the basis of a affiliations
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

neighbors.vector <- function(x, graph){
  
  
  
}

# Data coding and recoding -----

#' Combine descriptions
#' 
#' Combine all descriptions into a single character vector
#' @param x the name of an individual
#' @param rel.all an affiliation edge list
#' @return a character vector

beskrivelser <- function(x, rel.all = rel.all){
  besk       <- rel.all$DESCRIPTION[rel.all$NAME == x]
  org        <- rel.all$AFFILIATION[rel.all$NAME == x]
  paste(org, ":", besk)
}

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
  if(ignore.case==TRUE) beskrivelse <- tolower(beskrivelse)
  
  grep.soeg  <- paste(soegeord, collapse="|")
  grep.fund <- grep(grep.soeg, beskrivelse, ignore.case=ignore.case, ...)  
  # grep.fund <- grep(grep.soeg, beskrivelse, ignore.case=ignore.case)  
  navne.fund <- levels(as.factor(rel$NAME[grep.fund]))
  navne.ind  <- which(rel$NAME %in% navne.fund)
  
  droplevels(rel[navne.ind,])
}



#' Find the gender by name
#' 
#' Guesses the gender for a list of names by comparing it to the national distribution of first names.
#' @param navne a character vector of full names
#' @param names.gender a matrix with national distributions of first names
#' @return a factor with a gender guess
#' @export

find.gender <- function(navne){
  names.gender    <- soc.elite:::names.gender[, c(1,5)]
  Encoding(names.gender$Navn) <- "UTF-8" 
  n.list          <- strsplit(navne, " ")
  fornavne        <- sapply(n.list, head, 1)
  fornavne        <- data.frame(Navn = I(toupper(fornavne)))
  koen            <- dplyr::left_join(fornavne, names.gender, by = "Navn")
  b               <- c(0, 0.2, 0.8, 1)
  kategori        <- cut(koen[, 2], b, include.lowest=TRUE, labels=c("Women", "Binominal", "Men"))
  kategori
}

#' Extract first names
#' 
#' Extract first names from full names
#' @param navne a character vector of full nmaes
#' @return a character vector of first names
#' @export

fornavne      <- function(navne){
  navne           <- as.character(navne)
  n.list          <- strsplit(navne, " ")
  fornavne        <- sapply(n.list, head, 1)
  fornavne
}

#' Extract last names
#' 
#' Extract last names from full names
#' @param navne a character vector of full nmaes
#' @return a character vector of last names
#' @export

efternavne    <- function(navne){
  navne           <- as.character(navne)
  n.list          <- strsplit(navne, " ")
  efternavne      <- sapply(n.list, tail, 1)
}

#' Categories from postal codes
#' 
#' @param x a numeric vector with 4 digit danish postal codes
#' @return a data.frame with various factors
#' @export

inddel.postnummer <- function(x){
  postnumre <- postnumre[duplicated(postnumre$POSTNR)==FALSE,]
  jx <- data.frame(POSTNR = x)
  xm <- join(jx, postnumre, by = "POSTNR")
  xm
}


geographical.neighbours <- function(x){
  
}

find.geoposition <- function(x){
  
}
