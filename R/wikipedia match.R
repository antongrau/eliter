match_to_wikipedia <- function(x, timeout.seconds = 30, save_html = FALSE, wiki.language = "en", check.names = FALSE){
  # should it clean x?  
  # If it has & sign it should enquote it.
  
  o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
                    "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA, "wikidata_description" = NA) %>%
    mutate_at(vars(-is_a_hit), as.character)
  
  if(grepl("&", x, fixed = TRUE) & identical(check.names, TRUE)) x <- paste0('"', x, '"')
  
  url     <- paste0("https://", wiki.language, ".wikipedia.org/wiki/Special:Search?search=", x) %>% URLencode()
  
  got    <- tryCatch({
    url %>% GET(., timeout(timeout.seconds))},
    error = function(e) return(NA))
  
  # Translating to html
  wiki <- got %>% read_html(.)
  
  o["search_url"] <- url
  
  # When there is no hit: Break and return NA
  if(identical(is.na(wiki), TRUE))  return(o)
  
  
  o$is_a_hit <- TRUE
  
  # Perfect hit scenario  
  link.type <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("rel")
  link      <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("href")
  hit       <- link[link.type == "canonical"]
  hit       <- hit %>% gsub(".*wiki/(.*)", "\\1" , .)
  
  
  # Not a perfect hit 
  if(grepl("Special:Search?", hit, fixed = TRUE)){
    o["is_a_hit"]  <- FALSE 
    search.results <- wiki %>% html_elements("body") %>% html_elements("li") %>% html_elements("div") %>% html_elements("a") %>% html_attr("href")
    hit            <- search.results[1] %>% gsub("/wiki/", "", ., fixed = TRUE)
    
    if(identical(is.na(hit), TRUE))  return(o)
    
    url            <- paste0("https://", wiki.language, ".wikipedia.org/wiki/Special:Search?search=", hit)
    got    <- tryCatch({
      url %>% GET(., timeout(timeout.seconds))},
      error = function(e) return(NA))
    
    wiki <- got %>% read_html(.)
    
    link.type <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("rel")
    link      <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("href")
    hit       <- link[link.type == "canonical"]
    hit       <- hit %>% gsub(".*wiki/(.*)", "\\1" , .)
  }
  
  # Output
  
  o["wiki_handle"]     <- hit
  o["wiki_url"]        <- link[link.type == "canonical"]
  o["wiki_title"]      <- wiki %>% html_elements("body") %>% html_elements("#firstHeading") %>% html_text()
  content              <- wiki %>% html_elements("body") %>% html_elements("#mw-content-text") %>% html_elements("p")
  o["first_paragraph"] <- head(content, 10) %>% html_text() %>% paste(collapse = " ") %>% strtrim(1000)
  
  # Look up the wikidata id of a wikipage
  url <- paste0("https://", wiki.language, ".wikipedia.org/w/api.php?action=query&prop=pageprops&titles=", o$wiki_title ,"&format=json") %>% URLencode()
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  o["wikidata_qid"]         <- got$query$pages[[1]]$pageprops$wikibase_item
  o["wikidata_description"] <- got$query$pages[[1]]$pageprops$`wikibase-shortdesc`
  # Extracting the pageid 
  o["pageID"]               <- got$query$pages[[1]]$pageid
  
  # Save HTML ----
  if(identical(save_html, TRUE)) o <- c(o, wiki_html = list(wiki))
  
  o
}

match_to_wikidata <- function(x, timeout.seconds = 30, save_html = FALSE, wiki.language = "en", check.names = TRUE, key.words = NULL, valid.qids = NULL){
  # Link to documentation of the wikidata api: https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities
  # If it has & sign it should enquote it.
    o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
                    "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA) %>%
    mutate_at(vars(-is_a_hit), as.character)
  
  # Search wikidata
  if(identical(check.names, TRUE)) x <- fix_the_search_title_for_wiki_URL(x)

  url     <- paste0("https://www.wikidata.org/w/api.php?action=wbsearchentities&search=", x,
                    "&language=", wiki.language ,"&format=json&limit=50") 
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  o["search_url"] <- url
  
  if(identical(is.na(got), TRUE))  return(o)
  
  wikidata    <- got$search
  
  if(length(wikidata) == 0)  return(o)
  
  if(is.null(valid.qids) == FALSE){
    wikidata    <- wikidata[wikidata$id %in% valid.qids, ]
    
  }
  
  # Only those with a keyword in their description

  if(is.null(valid.qids) == TRUE | is.na(wikidata$id[1])){
    wikidata    <- got$search
    wikidata$text <- paste(wikidata$description, wikidata$label)
    wikidata    <- wikidata[grepl(paste(key.words, collapse = "|"), wikidata$text, ignore.case = TRUE),]
  }
  # Take the first hit of those
  hit.id  <- wikidata$id[1]
  
  if(is.na(hit.id) == TRUE) return(o)
  
  # Assign what we know until now
  o["wikidata_description"] <- wikidata$description[1]
  o["wikidata_qid"]         <- hit.id
  
  # Get data from wikipedia
  sites  <- paste0(unique(c(wiki.language, "en", "es", "de", "fr")), "wiki")
  sites.collapse <- sites %>% paste(collapse = "|")
  url    <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", hit.id, "&sitefilter=", sites.collapse,"&props=sitelinks&format=json")
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  site.links <- got$entities[[1]]$sitelinks[sites] %>% compact()
  
  if(length(site.links) == 0) return(o)
  
  wiki.title    <- site.links[[1]]$title
  wiki.language <- site.links[[1]]$site %>% str_remove("wiki") 
  
  if(identical(is.na(wiki.title), TRUE) | is.null(wiki.title))  return(o)
  
  wiki.title <- fix_the_search_title_for_wiki_URL(wiki.title)
  
  url   <- paste0("https://", wiki.language,".wikipedia.org/w/api.php?action=query&prop=extracts&titles=", wiki.title,
                  "&exlimit=max&exintro&exchars=500&format=json") %>% URLencode()
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  wiki                  <- got$query$pages[[1]]
  
  if(identical(is.na(got), TRUE) | is.null(wiki))  return(o)
  
  
  # Assign data from wikipedia
  o["wiki_handle"]     <- wiki$title
  o["wiki_title"]      <- wiki$title
  o["first_paragraph"] <- wiki$extract
  o["wiki_url"]        <- paste0("https://", wiki.language,".wikipedia.org/wiki/", wiki$title)
  o["pageID"]          <- wiki$pageid
  o["is_a_hit"]        <- TRUE
  
  o
}

match_to_wikidata_special_search <- function(x, timeout.seconds = 30, save_html = FALSE, wiki.language = "en", key.words = NULL, valid.qids = NULL){
  
  o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
                    "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA) %>%
    mutate_at(vars(-is_a_hit), as.character)
  
  
  u.start <- "https://www.wikidata.org/w/index.php?sort=relevance&search="
  x.or    <-  str_split(x, " ") %>% unlist %>% paste(., collapse = "+OR+")
  u.title <- "&title=Special:Search&profile=advanced&fulltext=1&advancedSearch-current={%22fields%22%3A{%22or%22%3A[%22"
  x.and   <-  str_split(x, " ") %>% unlist %>% paste0(., collapse = "%22%2C%22")
  u.end   <- "%22]}}&ns0=1&ns120=1"
  
  url     <- paste0(u.start, x.or, u.title, x.and, u.end)  
  
  
  # Search wikidata
  # if(identical(check.names, TRUE)) x <- fix_the_search_title_for_wiki_URL(x)
  
  
  got    <- tryCatch({
    url %>% URLencode() %>% GET},
    error = function(e) return(NA))
  
  o["search_url"] <- url
  
  if(identical(is.na(got), TRUE))  return(o)
  if(identical(got$status_code, as.integer(400)))  return(o)
  
  w      <- got %>% read_html()
  hits   <- w %>% html_elements(".mw-search-result")
  
  if(length(hits) == 0)  return(o)
  
  
  titles      <- hits %>% html_elements(".mw-search-result-heading") %>% html_elements("span") %>% html_elements(".wb-itemlink-label") %>% html_text()
  id          <- hits %>% html_elements(".mw-search-result-heading") %>% html_elements("span") %>% html_elements(".wb-itemlink-id") %>% html_text() %>% gsub(pattern = "\\(|\\)", x = ., replacement = "")
  text        <- hits %>% html_elements(".mw-search-result-heading") %>% html_elements("a") %>% html_attr("title")
  
  wikidata    <- tibble(titles, id, text)
  
  
  
  if(is.null(valid.qids) == FALSE){
    wikidata    <- wikidata[wikidata$id %in% valid.qids, ]
  }
  
  # Only those with a keyword in their description
  
  if(is.null(valid.qids) == TRUE | is.na(wikidata$id[1])){
    wikidata    <- wikidata[grepl(paste(key.words, collapse = "|"), wikidata$text, ignore.case = TRUE),]
  }
  # Take the first hit of those
  hit.id  <- wikidata$id[1]
  
  if(is.na(hit.id) == TRUE) return(o)
  
  # Assign what we know until now
  o["wikidata_description"] <- wikidata$text[1]
  o["wikidata_qid"]         <- hit.id
  
  # Get data from wikipedia
  sites  <- paste0(unique(c(wiki.language, "en", "es", "de", "fr")), "wiki")
  sites.collapse <- sites %>% paste(collapse = "|")
  url    <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", hit.id, "&sitefilter=", sites.collapse,"&props=sitelinks&format=json")
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  site.links <- got$entities[[1]]$sitelinks[sites] %>% compact()
  
  if(length(site.links) == 0) return(o)
  
  wiki.title    <- site.links[[1]]$title
  wiki.language <- site.links[[1]]$site %>% str_remove("wiki") 
  
  if(identical(is.na(wiki.title), TRUE) | is.null(wiki.title))  return(o)
  
  wiki.title <- fix_the_search_title_for_wiki_URL(wiki.title)
  
  url   <- paste0("https://", wiki.language,".wikipedia.org/w/api.php?action=query&prop=extracts&titles=", wiki.title,
                  "&exlimit=max&exintro&exchars=500&format=json") %>% URLencode()
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  wiki                  <- got$query$pages[[1]]
  
  if(identical(is.na(got), TRUE) | is.null(wiki))  return(o)
  
  
  # Assign data from wikipedia
  o["wiki_handle"]     <- wiki$title
  o["wiki_title"]      <- wiki$title
  o["first_paragraph"] <- wiki$extract
  o["wiki_url"]        <- paste0("https://", wiki.language,".wikipedia.org/wiki/", wiki$title)
  o["pageID"]          <- wiki$pageid
  o["is_a_hit"]        <- TRUE
  
  o
}



match_to_wikipedia_with_json <- function(x, timeout.seconds = 30, save_html = FALSE, wiki.language = "en", check.names = FALSE){
  # should it clean x?  
  # If it has & sign it should enquote it.
  
  o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
                    "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA) %>%
    mutate_at(vars(-is_a_hit), as.character)
  
  if(grepl("&", x, fixed = TRUE) & identical(check.names, TRUE)) x <- paste0('"', x, '"')
  
  url     <- paste0("https://", wiki.language,
                    ".wikipedia.org/w/api.php?format=xml&action=query&prop=extracts&generator=search&gsrsearch=", x,
                    "&exlimit=max&exintro&exchars=500&format=json") %>% URLencode()
  
  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  o["search_url"] <- url
  
  
  wiki  <- got$query$pages
  # When there is no hit: Break and return NA
  if(identical(is.na(got), TRUE) | is.null(wiki))  return(o)
  
  # Getting the top hit
  index <- wiki %>% pluck("index") %>% unlist()
  wiki  <- wiki[[which(index == 1)]]
  
  o["wiki_handle"]     <- wiki$title
  o["wiki_title"]      <- wiki$title
  o["first_paragraph"] <- wiki$extract
  o["pageID"]          <- wiki$pageid
  o["wiki_url"]        <- paste0("https://en.wikipedia.org/wiki/", wiki$title)
  
  o
}



match_to_wikipedia_with_google <- function(x, timeout.seconds = 30, save_html = FALSE){
  
  o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
                    "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA)  %>%
    mutate_at(vars(-is_a_hit), as.character)
  
  url     <- paste0("https://www.googleapis.com/customsearch/v1?key=AIzaSyCwPTphV-svyoGSQu0xBujwhZtXSodabBY&cx=004983950661609638858:mjbkvt7lgh6&q=", x) %>% URLencode()
  
  goog    <- tryCatch({
    url %>% fromJSON(url)},
    error = function(e) return(NA))
  
  o["search_url"] <- url
  
  # If there is no hit - then we return o without further ado.
  
  if(is.null(goog$items[[1]]$link)) return(o)
  
  # Scenario : We got a hit
  o$wiki_url    <- goog$items[[1]]$link
  o$wiki_handle <- goog$items[[1]]$link %>% gsub(".*wiki/(.*)", "\\1" , .)
  
  wiki    <- tryCatch({
    o$wiki_url %>% GET(., timeout(timeout.seconds)) %>% read_html(.)},
    error = function(e) return(NA))
  
  if(identical(is.na(wiki), TRUE)) return(o)
  
  o["wiki_title"]      <- wiki %>% html_elements("body") %>% html_elements("#firstHeading") %>% html_text()
  content              <- wiki %>% html_elements("body") %>% html_elements("#mw-content-text") %>% html_elements("p")
  o["first_paragraph"] <- head(content, 10) %>% html_text() %>% paste(collapse = " ") %>% strtrim(1000)
  
  # Save HTML ----
  if(identical(save_html, TRUE)) o <- c(o, wiki_html = list(wiki))
  
  o
  
}

check.wiki.match.quality <- function(wiki, key.words = c("corporation"), blacklist = c("this is a list of", "may also refer to", "commonly refers to"), language = "en"){
  
  text                       <- paste(wiki$first_paragraph, wiki$wikidata_description)
  
  # Black list text
  wiki$test.blacklist.text   <- text %>% grepl(paste(blacklist, collapse = "|"), ignore.case = TRUE, .)
  
  # Categories
  wiki$categories            <- get_categories_from_wiki(wiki$pageID, language) %>% as.character()
  
  # Black list categories 
  wiki$test.blacklist.cat    <- wiki$categories %>% grepl(paste(blacklist, collapse = "|"), ignore.case = TRUE, .)
  
  # Does it match keywords
  wiki$test.key.word.match   <- text %>% grepl(paste(key.words, collapse = "|"), ignore.case = TRUE, .)
  
  # Does it match keywords in categories
  wiki$test.key.categories   <- wiki$categories %>% grepl(paste(key.words, collapse = "|"), ignore.case = TRUE, .)
  
  test                       <- wiki %>% select(test.blacklist.cat, test.blacklist.text) %>% rowSums(.)
  test.positive              <- wiki %>% select(test.key.word.match, test.key.categories) %>% rowSums(.)
  wiki$redo                  <- test > 0 | test.positive == 0
  
  wiki
}

search_and_match_to_wiki <- function(match, key.words, blacklist = c("this is a list of", "may also refer to", "commonly refers to"), valid.qids = NULL, language = "en"){
  # It should fill out the wikidata stuff for those where it couldn't find it in the original lookup.
  stopifnot(length(match) <= 10)
  
  wiki         <- vector("list", length = length(match)) %>% map(as.character)
  names(wiki)  <- match
  
  # Wikidata ----
  j            <- 1 
  for(i in j:length(match)){
    j         <- i                                                       # We use i and j because we can then restart the loop from j - if it fails or we have to pause it.
    hit       <- match_to_wikidata(match[i], check.names = TRUE, key.words = key.words, valid.qids = valid.qids, wiki.language = language)
    cat("\n", i,". ", hit$original, " = ", hit$wiki_handle, "(", "Wikidata hit =", hit$is_a_hit ,")", "\n")
    wiki[[i]] <- hit
    }
  
  wiki             <- bind_rows(wiki)
  wiki$matchtype   <- "Wikidata"
  wiki             <- check.wiki.match.quality(wiki, key.words = key.words, blacklist = blacklist, language = language)
  
  if(any(wiki$redo) == FALSE) return(wiki)
  
  # Wikipedia ----
  wiki.first  <- wiki %>% filter(redo)
  match       <- wiki.first$original
  
  wiki.list         <- vector("list", length = length(match))
  names(wiki.list)  <- match
  
   
  j            <- 1 
  for(i in j:length(match)){
    j         <- i                                                       # We use i and j because we can then restart the loop from j - if it fails or we have to pause it.
    hit       <- match_to_wikipedia(match[i], check.names = TRUE, wiki.language = language)
    cat("\n", i,". ", hit$original, " = ", hit$wiki_handle, "(", "Wikipedia first try Hit =", hit$is_a_hit ,")", "\n")
    wiki.list[[i]] <- hit
    }
  
  wiki.first             <- bind_rows(wiki.list)
  wiki.first$matchtype   <- "First wikipedia"
  
  # Check quality
  wiki.first                           <- check.wiki.match.quality(wiki.first, key.words = key.words, blacklist = blacklist, language = language)
  
  # Join wikidata and first try.
  
  wiki            <- anti_join(wiki, wiki.first, "original") %>% bind_rows(wiki.first) 
  
  
  if(any(wiki.first$redo) == FALSE) return(wiki)
  
  # Wikidata special ----
  
  # Separate and create search terms
  wiki.special    <- wiki %>% filter(redo)
  
  
  
  # Create new search term
  match             <- wiki.special$original
  wiki.list         <- vector("list", length = length(match))
  names(wiki.list)  <- wiki.special$original
  
  
  j            <- 1 
  for(i in j:length(match)){
    j         <- i                                                       # We use i and j because we can then restart the loop from j - if it fails or we have to pause it.
    hit       <- match_to_wikidata_special_search(match[i], key.words = key.words, valid.qids = valid.qids, wiki.language = language)
    cat("\n", i,". ", hit$original, " = ", hit$wiki_handle, "(", "Wikidata special redo Hit =", hit$is_a_hit ,")", "\n")
    wiki.list[[i]] <- hit
  }
  
  wiki.special.hit    <- bind_rows(wiki.list, .id = "wiki_id") 
  wiki.special.hit    <- check.wiki.match.quality(wiki.special.hit, key.words = key.words, blacklist = blacklist, language = language)
  wiki.special.hit    <- wiki.special.hit %>% select(-original) %>% rename(original = wiki_id)
  
  wiki.special.hit$matchtype   <- "Wikidata special"
  
  
  
  # Join back
  wiki            <- anti_join(wiki, wiki.special.hit, "original") %>% bind_rows(wiki.special.hit) 
  
  # Get extra data in bulk 

  wiki       <- get_wikidata_description(wiki, language)
  wiki
  
}

get_wikidata_description <- function(wiki, language = language){
  x      <- wiki$wikidata_qid
  xs     <- paste0(unique(na.omit(x)), collapse = "|")
  url    <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", xs,"&props=descriptions&languages=", language,"&format=json") %>% URLencode()

  got    <- tryCatch({
    url %>% fromJSON()},
    error = function(e) return(NA))
  
  o       <- got$entities %>% pluck("descriptions") %>% map(~pluck(.x, "value")) %>% bind_rows(.id = "wikidata_qid")
  
  if(nrow(o) == 0) return(wiki)
  
  desc    <- tibble(wikidata_qid = o$wikidata_qid, o = o[, 2])
  
  
  if(is.null(wiki$wikidata_description) == FALSE) wiki <-  wiki %>% select(-wikidata_description)
  wiki       <- wiki %>% left_join(., desc, by = "wikidata_qid")
  wiki
  
}


get_categories_from_wiki <- function(x, language){
  
  stopifnot(length(x) <= 10)
  x.o     <- na.omit(x)
  url     <- paste0("https://", language,".wikipedia.org/w/api.php?format=jsonfm&action=query&pageids=", paste0(x.o, collapse = "|"), "&prop=categories&cllimit=max&format=json") %>% URLencode()
  wiki    <- tryCatch({jsonlite::fromJSON(url)},
                      error = function(e) return(NA))
  
  pages     <- wiki$query$pages
  anycat    <- pages %>% pluck("categories") %>% compact()
  
  if(is.null(pages) | length(anycat) == 0) return(NA)
  
  cat     <- pages %>% pluck("categories") %>% pluck("title") %>% map(as_tibble) %>% bind_rows(.id = "x")
  cat     <- cat %>% mutate(x = as.numeric(x), value = str_remove(value, pattern = "Category:"))
  cat     <- cat %>% group_by(x) %>% summarise(categories = paste(value, collapse = "; "))
  out     <- tibble(x) %>% left_join(y = cat)
  out$categories
}





#' Match search terms to Wikipedia
#'
#' @param x a character vector
#' @param file a filepath
#' @param chunk.size the number of search terms in each chunk - it cannot be above 50
#' @param save.every the number of chunks between each save to file
#' @param key.words a character vector with the keywords that should be present in a wiki article or in the wiki categories
#' @param blacklist a character vector with blacklisted terms or phrases.
#'
#' @return a tibble
#' @export
#'
#' @examples

match_and_write_chunks_from_wiki <- function(x, file = "saved/wiki_match.Rda", chunk.size = 10, save.every = 10, key.words = c("city"), blacklist = c("is a list of", "may also refer to", "commonly refers to"), valid.qids = NULL, language = "en"){
  
  wiki_match         <- NULL
  # Does file exits
  
  # Create file
  
  # Read file and find out how much we need to do
  if(file.exists(file)){
    load(file = file)
    cat("\n", "Rows from ", file, "not in x", sum(!wiki_match$original %in% x), "\n")
    cat("\n", "Rows from x not in", file, sum(!x %in% wiki_match$original), "\n")
    x                    <- x[!x %in% wiki_match$original]
  }
  
  if(length(x) == 0) return(wiki_match)
  
  # Partition into chunks 
  x.l            <- split(x, ceiling(seq_along(x)/chunk.size))
  save.sequence  <- seq(1, length(x), by = save.every)
  
  # The matching loop
  pb <- txtProgressBar(0, length(x.l), style = 3)

  for(i in 1:length(x.l)){
    setTxtProgressBar(pb, i)                                                                         # Progress bar
    
    cat("\n", "Chunk", i, "of", length(x.l), "\n",
        paste(x.l[[i]], "\n"), "\n")
    
    o              <- search_and_match_to_wiki(x.l[[i]], key.words = key.words, blacklist = blacklist, valid.qids = valid.qids, language = language)
    wiki_match     <- bind_rows(wiki_match, o)
    if(i %in% save.sequence) save(wiki_match, file = file)
    }

  save(wiki_match, file = file)
  wiki_match
  
}

fix_the_search_title_for_wiki_URL <- function(x){
  x  <- gsub(pattern = "-", replacement = "", x = x, fixed = TRUE)  
  x  <- stringr::str_squish(x)
  x  <- URLencode(x)
  x  <- gsub(pattern = "&", replacement = "%26", x = x, fixed = TRUE)  
  
#  if(grepl(" ", x, fixed = TRUE))   x <- paste0('"', x, '"')  
  
  x
}


get_wikidata_qid_from_wikipedia_title <- function(x, chunk.size = 500){
  
  x.l            <- split(x, ceiling(seq_along(x)/chunk.size))
  l.o            <- list()
  
  # The matching loop
  pb  <- txtProgressBar(0, length(x.l), style = 3)
  
  
  for(i in 1:length(x.l)){
    setTxtProgressBar(pb, i)                                                                         # Progress bar
    
    w <- paste0(x.l[[i]], collapse = "|")
    url     <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=", w,
                      "&format=json") 
    
    got    <- tryCatch({
      url %>% fromJSON()},
      error = function(e) return(NA))
    
    pages      <- got$query$pages
    o          <- bind_rows(pages) %>% select(-ns)
    o$pageprops[map_lgl(o$pageprops, is.null)] <- NA
    o          <- o %>% unnest(pageprops)
    o$original <- x.l[[i]]
    o$missing[is.na(o$missing) == FALSE] <- TRUE
    l.o[[i]]   <- o
  }
  bind_rows(l.o)
  
}



# OLD STUFF ----
# match_to_wikipedia <- function(x, timeout.seconds = 30, save_html = FALSE, wiki.language = "en", check.names = FALSE){
#   # should it clean x?  
#   # If it has & sign it should enquote it.
#   
#   o       <- tibble("original" = x, "wiki_handle" = NA, "wiki_url" = NA, 
#                     "is_a_hit" = NA, "wiki_title" = NA, "first_paragraph" = NA, "search_url" = NA)
#   
#   if(grepl("&", x, fixed = TRUE) & identical(check.names, TRUE)) x <- paste0('"', x, '"')
#   
#   url     <- paste0("https://", wiki.language, ".wikipedia.org/wiki/Special:Search?search=", x) %>% URLencode()
#   
#   got    <- tryCatch({
#     url %>% GET(., timeout(timeout.seconds))},
#     error = function(e) return(NA))
#   
#   # Translating to html
#   wiki <- got %>% read_html(.)
#   
#   o["search_url"] <- url
#   
#   # When there is no hit: Break and return NA
#   if(identical(is.na(wiki), TRUE))  return(o)
#   
#   
#   o$is_a_hit <- TRUE
#   
#   # Perfect hit scenario  
#   link.type <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("rel")
#   link      <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("href")
#   hit       <- link[link.type == "canonical"]
#   hit       <- hit %>% gsub(".*wiki/(.*)", "\\1" , .)
#   
#   
#   # Not a perfect hit 
#   if(grepl("Special:Search?", hit, fixed = TRUE)){
#     o["is_a_hit"]  <- FALSE 
#     search.results <- wiki %>% html_elements("body") %>% html_elements("li") %>% html_elements("div") %>% html_elements("a") %>% html_attr("href")
#     hit            <- search.results[1] %>% gsub("/wiki/", "", ., fixed = TRUE)
#     
#     if(identical(is.na(hit), TRUE))  return(o)
#     
#     url            <- paste0("https://", wiki.language, ".wikipedia.org/wiki/Special:Search?search=", hit)
#     got    <- tryCatch({
#       url %>% GET(., timeout(timeout.seconds))},
#       error = function(e) return(NA))
#     
#     wiki <- got %>% read_html(.)
#     
#     link.type <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("rel")
#     link      <- wiki %>% html_elements("head") %>% html_elements("link") %>% html_attr("href")
#     hit       <- link[link.type == "canonical"]
#     hit       <- hit %>% gsub(".*wiki/(.*)", "\\1" , .)
#   }
#   
#   # Output
#   
#   o["wiki_handle"]     <- hit
#   o["wiki_url"]        <- link[link.type == "canonical"]
#   o["wiki_title"]      <- wiki %>% html_elements("body") %>% html_elements("#firstHeading") %>% html_text()
#   content              <- wiki %>% html_elements("body") %>% html_elements("#mw-content-text") %>% html_elements("p")
#   o["first_paragraph"] <- head(content, 10) %>% html_text() %>% paste(collapse = " ") %>% strtrim(1000)
#   
#   Q                    <- wiki %>% html_text() %>% str_extract("www\\.wikidata\\.org\\\\\\/entity\\\\\\/Q[0-9]{1,}") # Det her er fucked up!
#   Q                    <- names(tail(sort(table(Q)),1))
#   o["wikidata_qid"]    <- Q
#   # Extracting the pageid 
#   o["pageID"]          <- got$headers$`x-analytics` %>% str_extract("page_id=[0-9]{1,}") %>% str_extract("[0-9]{1,}") %>% as.numeric()
#   
#   
#   # Save HTML ----
#   if(identical(save_html, TRUE)) o <- c(o, wiki_html = list(wiki))
#   
#   o
# }
