# Search ---

#' What?
#' 
#' Returns the list of members of the affiliations. Names are matched via grep.
#' @param affil a character string with the name of one or several affiliations
#' @param den an affiliation edge list in a similar format to \link{den}, if "den" the den dataset is used.
#' @param ignore.case if TRUE grep is not case sensitive
#' @param ... further arguments are passed on to \link{grep}
#' @return A matrix with names and affiliation
#' @export

what          <- function(affil, den = "den", ignore.case = TRUE, tags = FALSE, ...){
  
  if (identical(den, "den")) data(den, envir = environment())
  
  pattern     <- paste(affil, collapse = "|")
  found       <- grep(pattern, den$AFFILIATION, ignore.case = ignore.case, ...)
  den.found   <- den[found,]
  out         <- data.frame(Name = den.found$NAME, Affiliation = den.found$AFFILIATION, Role = den.found$ROLE)
  if(identical(tags, TRUE)) out         <- data.frame(out, TAGS = den.found$TAGS)
  out         <- sapply(out, as.character)
  out[is.na(out)] <- ""
  noquote(out)
}

#' Who?
#' 
#' Returns the affiliation memberships of an individual, or all direct contacts. Names are matched with grep.
#' @param name the name of the individual
#' @param den an affiliation edge list in a similar format to \link{den}, if "den" the den dataset is used.
#' @param only.affiliations if TRUE returns the affiliations of the individual
#' @param ignore.case if TRUE grep is not case sensitive
#' @param if TRUE tags are returned
#' @param ... further arguments are passed on to \link{grep}
#' @return A matrix with names and affiliation
#' @export

who          <- function(name, den = "den", only.affiliations = TRUE, ignore.case = TRUE, tags = FALSE, ...){
  
  if (identical(den, "den")) data(den, envir = environment())
  
  pattern     <- paste(name, collapse = "|")
  found       <- grep(pattern, den$NAME, ignore.case = ignore.case)
  found.names <- unique(as.character(den$NAME[found]))
  found.affil <- den$AFFILIATION[found]
  den.found   <- den[den$AFFILIATION %in% found.affil,]
  out         <- data.frame(Name = den.found$NAME, Affiliation = den.found$AFFILIATION, Role = den.found$ROLE)
  if (identical(tags, TRUE)) out         <- data.frame(out, TAGS = den.found$TAGS)
  if (identical(only.affiliations, TRUE)) {
    out <- out[out$Name %in% found.names,]
    out <- out[duplicated(data.frame(out$Name, out$Affiliation)) == FALSE,]
  }
  out         <- sapply(out, as.character)
  out[is.na(out)] <- ""
  rownames(out) <- NULL
  noquote(out)
}

described.as <- function(x, den){
  
  
  
}
