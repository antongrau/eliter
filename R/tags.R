# Tags and sectors -----

#' Has.tags
#' 
#' Selects all affiliations that have one or several tags
#' @param den an affiliation edge list
#' @param tags is a character vector of tags
#' @param result if "den" \code{has.tags} returns a character vector with unique affiliation names. If result is "name", a character vector of unique names is returned. If result is "den", a den object is returned.
#' @param silent if TRUE the table with the number of matched positions per tag is not shown.
#' @export
#' @examples
#' data(den)
#' has.tags(den, tags = c("Youth", "Children"))
#' has.tags(den, tags = c("Youth", "Children"), res = "name")
#' has.tags(den, tags = c("Youth", "Children"), res = "den")

has.tags       <- function(den, tags, result = c("affil", "name", "den") , silent = FALSE){
  
  find.in.tag.list       <- function(tag.list, tag) unlist(lapply(tag.list, function(x, tag) any(x == tag), tag = tag))
  
  result.args            <- c("affil", "name", "den")
  affil.name             <- as.character(den$AFFILIATION)
  tag.list               <- strsplit(as.character(den$TAGS),  ",")
  
  # Remove white space noise
  tags                   <- trimws(tags)
  tag.list               <- lapply(tag.list, trimws) 
  
  # Find tags
  sector.match           <- do.call("cbind", lapply(tags, find.in.tag.list, tag.list = tag.list))
  colnames(sector.match) <- tags
  
  tag.affils             <- unique(affil.name[which(rowSums(sector.match) >= 1)])
  
  # Return amount of matches per tag
  if (identical(silent, FALSE)) {
    how.many.per.tag     <- cbind("Matched positions" = colSums(sector.match, na.rm = T))
    print(how.many.per.tag)
    cat("\n", "\n")
  }
  
  # Results
  
  if (match.arg(result, result.args) == "affil") {
    return(as.character(tag.affils))
  }
  
  if (match.arg(result, result.args) == "name") {
    return(as.character(unique(den$NAME[den$AFFILIATION %in% tag.affils])))
  }
  
  if (match.arg(result, result.args) == "den") {
    droplevels(den[den$AFFILIATION %in% tag.affils,])
  }
}

#' show.all.tags
#' 
#' Displays a matrix with all the tags in den.
#' 
#' @param den a affiliation edgelist
#' @return a matrix with all tags and their frequencies
#' @export
#' @examples
#' data(den)
#' show.all.tags(den)

show.all.tags   <- function(den){
  tags                <- as.character(den$TAGS)
  tags.split          <- trimws(unlist(strsplit(tags, ",")))
  tags.positions      <- table(tags.split)
  tags.affiliations   <- tags[duplicated(den$AFFILIATION) == FALSE]
  tags.affiliations   <- table(trimws(unlist(strsplit(tags.affiliations, ","))))
  cbind(Positions = tags.positions, Affiliations = tags.affiliations)
}

#' Convert list of tags to sectors
#'
#' @param den a den class object
#' @param sector.tags a list of tags
#' @param other The other category, if FALSE, it is omitted
#' @param silent if FALSE the number of matched positions and affiliations is printed
#' @param mutually.exclusive if TRUE the produced sectors are mutually exclusive 
#' @param sector.membership if TRUE a data.frame with a mutually exclusive sector memberships vector is returned
#'
#' @return a list of den objects
#' @export
#'
#' @examples
#' data(den)
#' sectors        <- standard.sectors("Danish")
#' tags.to.sectors(den, sectors)

tags.to.sectors <- function(den, sector.tags, other = "Other", silent = FALSE, mutually.exclusive = FALSE, sector.membership = FALSE){
 
   # Match tags
  list.dens            <- lapply(sector.tags, has.tags, den = den, result = "den", silent = silent)
  
  # Other category
  if (other != FALSE) {
    affil.names        <- lapply(list.dens, getElement, "AFFILIATION")
    affil.names        <- unique(unlist(affil.names, use.names = F))
    other.affils       <- setdiff(unique(den$AFFILIATION), affil.names)
    den.other          <- droplevels(den[den$AFFILIATION %in% other.affils,])
    l                  <- length(list.dens)
    list.dens[[l + 1]] <- den.other
    names(list.dens)[l + 1] <- other
  }
  
  # Mutually exclusive
  affil.names                 <- lapply(list.dens, getElement, "AFFILIATION")
  affil.names                 <- sort(as.character(unique(unlist(affil.names, use.names = F))))
  
  sector.edge                 <- list()
  for (i in 1:length(list.dens)) sector.edge[[i]]  <- data.frame("AFFILIATION" = unique(as.character(list.dens[[i]]$AFFILIATION)), "SECTOR" = names(list.dens[i]))
  sector.edge                 <- do.call("rbind", sector.edge)
  incidence.sector            <- xtabs(~ AFFILIATION + SECTOR, sector.edge, sparse = T)
  
  sector.membership           <- vector(mode = "logical", length = nrow(incidence.sector))
  for (i in 1:ncol(incidence.sector)) sector.membership[incidence.sector[, i] == 1] <- colnames(incidence.sector)[i]
  
  exclusive.names             <- split(affil.names, f = sector.membership)
  for (i in 1:length(list.dens)) list.dens[[i]] <- list.dens[[i]][list.dens[[i]]$AFFILIATION %in% exclusive.names[[i]] ,  , drop = TRUE]

  # Out  
  
  if (sector.membership == TRUE & mutually.exclusive == TRUE) return(data.frame("AFFILIATION" = rownames(incidence.sector), "Sector" = sector.membership))
  
  list.dens
}

#' Standard sector tags
#'
#' Collections of tags that roughly correspond to sectors.
#'
#' @param sets a character vector with the sets of sector tags
#'
#' @return a list of tags
#' @export
#'
#' @examples
#' data(den)
#' sectors      <- standard.sectors()
#' sectors$Danish
#' 
#' standard.sectors("English")

standard.sectors <- function(sets = c("Danish", "English", "4 Sectors")){
  
  set.list                           <- list()
  
  # Danish and detailed set
  list.tags                           <- list()
  list.tags$Erhvervsliv               <- c("Corporation")
  list.tags$Arbejdsgivere             <- c("Business association", "Employers association")
  list.tags$"Fagforeninger"           <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
  list.tags$"Kultur og medier"        <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language")
  list.tags$"Videnskab og uddannelse" <- c("Science", "Education", "Universities") 
  list.tags$Stat                      <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business") # FEJL - State business
  list.tags$Politik                   <- c("Politics", "Parliament")
  list.tags$Sundhed                   <- c("Health", "Patients", "MEDI", "Medicine", "Doctors")
  list.tags$Fritid                    <- c("Recreation and sports", "Sports", "Recreation")
  list.tags$Fonde                     <- c("Foundation", "Charity")
  list.tags$"Fødevarer"               <- c("Farming", "Food", "Fishing", "LEVN", "Forestry")
  
  set.list$"Danish"                   <- list.tags
  
  
  # English and detailed set
    list.tags                           <- list()
    list.tags$Corporations              <- c("Corporation")
    list.tags$"Employers"               <- c("Corporation", "Business association", "Employers association")
    list.tags$"Unions"                  <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
    list.tags$"Culture and media"       <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language")
    list.tags$"Science and education"   <- c("Science", "Education", "Universities", "Museums")
    list.tags$State                     <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business") # FEJL - State business
    list.tags$Politics                  <- c("Politics", "Parliament")
    list.tags$Health                    <- c("Health", "Patients", "MEDI", "Medicine", "Doctors")
    list.tags$Recreation                <- c("Recreation and sports", "Sports", "Recreation")
    list.tags$Foundation                <- c("Foundation", "Charity")
    list.tags$"Foods"                   <- c("Farming", "Food", "Fishing", "LEVN", "Forestry")
  
    set.list$"English"                  <- list.tags
  
  # 4 Sectors
    
    list.tags                           <- list()
    list.tags$"State and politics"      <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business", "Politics", "Parliament") # FEJL - State business
    list.tags$"Business"                <- c("Corporation", "Business association", "Employers association")
    list.tags$"Unions"                  <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
    list.tags$"Science and culture"     <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language", "Science", "Education", "Universities", "Museums")
    
    set.list$"4 Sectors"                <- list.tags
  
  set.list                              <- set.list[sets]
  if (length(set.list) == 1) set.list   <- set.list[[1]]
  set.list
}


variables.to.tags <- function(den, variables){
  
}