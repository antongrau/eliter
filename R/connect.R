# Connect to databases

#' Create a den object from the elite database
#' 
#' Something something
#' @export


eliteDB.connections <- function(pass = ""){
  # Måske skal der laves noget datarens - tjek fx for tomme navne og NA.
  # Det ville nok også være sundt med nogle automatiske tests
  # Det ser ud til at ikke alle affiliation ids kan findes i connections 
  pass_string                     <- paste0("&password=", pass)
  elite.db.connections            <- fromJSON(paste0("http://elitedb.ogtal.dk/exporter.php?type=connections&database=elite", pass_string))
  elite.db.persons                <- fromJSON(paste0("http://elitedb.ogtal.dk/exporter.php?type=persons&database=elite", pass_string))
  elite.db.affil                  <- fromJSON(paste0("http://elitedb.ogtal.dk/exporter.php?type=affiliations&database=elite", pass_string))
  
  connections                     <- elite.db.connections[order(elite.db.connections$affiliation_id),]
  persons                         <- elite.db.persons[order(elite.db.persons$id),]
  affiliations                    <- elite.db.affil[order(elite.db.affil$id),]
  affiliations$id.x               <- affiliations$id
  
  persons$person_id               <- persons$id
  
  # Data rens
  persons                         <- persons[is.na(persons$fullname)==FALSE,]
  
  # Navne dupletter
  dup.navn                       <- persons$fullname[duplicated(persons$fullname)]
  dup.id                         <- persons$person_id[duplicated(persons$fullname)]
  persons$fullname_dup           <- persons$fullname
  persons$fullname_dup[duplicated(persons$fullname)]           <- paste(dup.navn, dup.id)
  
  # Merge
  
  connections                     <- merge(connections, persons, by = "person_id", all.x = T, sort = TRUE)
  connections                     <- merge(connections, affiliations, by.x = "affiliationname", by.y = "name", all.x = T, sort = TRUE)
  
  # Merge
  gender                          <- find.gender(navne = connections$fullname)
  levels(gender)                  <- c("Women", "Undefined", "Men")
  
  connections.den                 <- data.frame(NAME        = connections$fullname_dup,
                                                AFFILIATION = connections$affiliationname,
                                                ROLE        = connections$rolename,
                                                GENDER      = gender,
                                                DESCRIPTION = connections$description.x,
                                                SOURCE      = connections$affiliationsector,
                                                BIQ_LINK    = connections$biq,
                                                CVR         = as.numeric(connections$cvr),
                                                TAGS        = connections$tagnames,
                                                MODIFIED    = as.Date(connections$modified_date.y, "%Y-%m-%d"),
                                                CREATED     = as.Date(connections$created_date, "%Y-%m-%d"),
                                                ARCHIVED    = as.Date(connections$archived_date.x, "%Y-%m-%d"),
                                                PERSON_ID   = connections$person_id,
                                                PERSON_CVR  = as.numeric(connections$cvrid),
                                                DESCRIPTION_AFFILIATION = connections$description
  )
  connections.den
}


#' Create a den object from the elite cvr database
#' 
#' Something something
#' @export

cvrDB.connections <- function(pass = "", database = "bigdata"){
  # Affiliations matricen kan ikke komme ud.
  library(curl)
  library(httr)
  set_config(config(ssl_verifypeer = 0L))
  
  pass_string                     <- paste0("&password=", pass)
  
  elite.db.connections            <- fromJSON(paste0("https://elitedb.ogtal.dk/exporter.php?type=connections&database=", database, pass_string))
  elite.db.persons                <- fromJSON(paste0("https://elitedb.ogtal.dk/exporter.php?type=persons&database=", database, pass_string))
  elite.db.affil                  <- fromJSON(paste0("https://elitedb.ogtal.dk/exporter.php?type=affiliations&database=", database, pass_string))
  
  connections                     <- elite.db.connections[order(elite.db.connections$cvr),]
  persons                         <- elite.db.persons[order(elite.db.persons$enhedsnummer),]
  affiliations                    <- elite.db.affil[order(elite.db.affil$cvr),]
  affiliations$id.x               <- affiliations$cvr
  
  persons$person_id               <- persons$enhedsnummer
  connections$person_id           <- connections$enhedsnummer
  
  
  # Navne dupletter
  dup.navn                       <- persons$fullname[duplicated(persons$fullname)]
  dup.id                         <- persons$person_id[duplicated(persons$fullname)]
  persons$fullname_dup           <- persons$fullname
  persons$fullname_dup[duplicated(persons$fullname)]           <- paste(dup.navn, dup.id)
  
  persons$gender                          <- find.gender(navne = persons$fullname_dup)
  levels(persons$gender)                  <- c("Women", "Undefined", "Men")
  
  
  
  # Merge
  connections                      <- merge(connections, persons, by = "person_id", all.x = T, sort = TRUE)
  connections                      <- merge(connections, affiliations, by.x = "cvr", by.y = "cvr", all.x = T, sort = TRUE)
  head(connections, 100)
  # Merge
  connections.den                 <- data.frame(NAME        = connections$fullname_dup,
                                                AFFILIATION = connections$affiliationname,
                                                ROLE        = as.factor(connections$role),
                                                GENDER      = connections$gender,
                                                SOURCE      = "CVR_udtræk",
                                                CVR         = connections$cvr,
                                                TAGS        = connections$hovedbranchenavn,
                                                MODIFIED    = NA, #connections$modified_date.y,
                                                CREATED     = NA, #connections$created_date,
                                                ARCHIVED    = NA, #connections$archived_date.x,
                                                PERSON_ID   = NA, #connections$person_id,
                                                START_DATE  = as.Date(connections$startdate, "%Y-%m-%d"),
                                                END_DATE    = as.Date(connections$enddate, "%Y-%m-%d"),
                                                VALGFORM    = connections$valgform,
                                                PERSON_ADRESSE = connections$adresse.x,
                                                PERSON_POSTNR  = connections$postnummer.x,
                                                PERSON_KOMMUNE = as.factor(connections$kommune.x),
                                                PERSON_CVR  = connections$enhedsnummer.x,
                                                VIRK_START  = as.Date(connections$livsforloebstart, "%Y-%m-%d"),
                                                VIRK_SLUT   = as.Date(connections$livsforloebslut, "%Y-%m-%d"),
                                                VIRK_KOMMUNE = as.factor(connections$kommune.y),
                                                VIRK_ADRESSE_POSTNR = connections$postnummer.y,
                                                VIRK_ADRESSE = connections$adresse.y,
                                                VIRK_BRANCHE = connections$hovedbranchenavn,
                                                VIRK_BRANCHEKODE = connections$hovedbranchekode,
                                                stringsAsFactors = FALSE
  )
  connections.den
}



#' eliteDB_open
#'
#' Opret forbindelse til eliteDB
#' @details eliteDB_open bruges til at oprette en forbindelse til vores server, sådan at man kan begynde at hente data ned.
#'     Funktionen skal køres før man kan bruge de øvrige eliteDB funktioner (med undtagelse af eliteDB_den). 
#' @param user brugernavn
#' @param pass kodeord
#' @param db navnet på den database du ønsker at tilgå
#' @return en aktiv MySQL forbindelse til eliteDB
#' @seealso 
#'     \code{\link{eliteDB_close}} 
#'     \code{\link{eliteDB_list_tables}}
#'     \code{\link{eliteDB_read_table}}
#' @encoding utf-8
#' @export
#' @examples
#' conn <- eliteDB_open(user, pass)
#' eliteDB_list_tables(conn)
#' persons <- eliteDB_read_table(conn,"Persons")
#' eliteDB_close(conn)
eliteDB_open <- function(user, pass, db = "elitedb"){
  
  sysname <- Sys.info()['sysname']
  
  connection <-  DBI::dbConnect(RMySQL::MySQL(), 
                                username = user, 
                                password = pass,
                                host = "www.ogtal.dk", 
                                port = 3306, 
                                dbname = db
  )
  
  if (sysname != "Windows"){
    DBI::dbGetQuery(connection, 'SET NAMES utf8;')
  }
  return(connection)
} 

# eliteDB_close -----------------------------------------------------------

#' eliteDB_close
#'
#' Luk en aktiv forbindelse til eliteDB
#' @param conn en åben forbindelse til eliteDB
#' @seealso 
#'     \code{\link{eliteDB_open}} 
#'     \code{\link{eliteDB_list_tables}}
#'      \code{\link{eliteDB_read_table}}
#' @encoding utf-8
#' @export
#' @examples
#' conn <- eliteDB_open(user, pass)
#' eliteDB_list_tables(conn)
#' persons <- eliteDB_read_table(conn,"Persons")
#' eliteDB_close(conn)
eliteDB_close <- function(conn){
  RMySQL::dbDisconnect(conn)
} 

# eliteDB_read_table ------------------------------------------------------

#' eliteDB_read_table
#'
#' Hent data fra eliteDB
#' @param conn en åben forbindelse til eliteDB
#' @param table navn på den tabel der skal hentes.
#' @encoding utf-8
#' @export
#' @seealso 
#'     \code{\link{eliteDB_open}} 
#'     \code{\link{eliteDB_close}}
#'     \code{\link{eliteDB_list_tables}}
#' @encoding utf-8
#' @export
#' @examples
#' conn <- eliteDB_open(user, pass)
#' eliteDB_list_tables(conn)
#' persons <- eliteDB_read_table(conn,"Persons")
#' eliteDB_close(conn)
eliteDB_read_table <- function(conn, table){
  result <- RMySQL::dbReadTable(conn, table)
  return(result)
}

# eliteDB_list_tables -----------------------------------------------------

#' eliteDB_list_tables
#'
#' Se hvilke tabeler der er tilgængelige i eliteDB
#' @param conn en åben forbindelse til eliteDB
#' @encoding utf-8
#' @seealso 
#'     \code{\link{eliteDB_open}} 
#'     \code{\link{eliteDB_close}} 
#'     \code{\link{eliteDB_read_table}}
#' @encoding utf-8
#' @export
#' @examples
#' conn <- eliteDB_open(user, pass)
#' eliteDB_list_tables(conn)
#' persons <- eliteDB_read_table(conn,"Persons")
#' eliteDB_close(conn)
eliteDB_list_tables <- function(conn){
  result <- DBI::dbListTables(conn)
  return(result)
}

# eliteDB_den -------------------------------------------------------------

#' eliteDB_den
#'
#' Hent et komplet den-objekt fra eliteDB
#' @param conn en åben forbindelse til eliteDB
#' @param user brugernavn
#' @param pass kodeord
#' @encoding utf-8
#' @return Et komplet den-objekt, der både indeholder aktive og arkiverede forbindelser
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' den        <- eliteDB_den(user, password)
#' den.active <- den[is.na(den$ARCHIVED),]
eliteDB_den <- function(conn = NULL, user = NULL, pass = NULL) {
  
  if (is.null(conn)){
    conn <- eliteR::eliteDB_open(user, pass)
  }
  
  den <- DBI::dbGetQuery(
    conn,
    "SELECT
    c.id, c.person_id, c.affiliation_id, c.role_id, c.description, c.created_date, c.archived_date, p.cvrid, a.name, a.type, a.cvr, a.sector, a.last_checked
    FROM Connections c
    LEFT JOIN Persons p
    ON c.person_id=p.id
    LEFT JOIN Affiliations a
    ON c.affiliation_id=a.id"
  )
  
  tags <- DBI::dbGetQuery(
    conn,
    "SELECT
    t.affiliation_id, GROUP_CONCAT(tl.tag) AS tagnames
    FROM Tags t
    LEFT JOIN Taglist tl
    ON t.tag_id=tl.id
    GROUP BY t.affiliation_id"
  )
  
  rolelist <- DBI::dbGetQuery(conn, "SELECT * FROM Rolelist")
  
  persons <- DBI::dbGetQuery(conn, "SELECT p.id, p.alias FROM Persons p")
  
  # Provide all non-unique aliases with an id sufix
  persons$alias <- gsub("\\d", "", x = persons$alias) %>% stringr::str_trim()
  dup <- duplicated(persons$alias) | duplicated(persons$alias, fromLast = TRUE)
  persons$alias[dup] <- paste(persons$alias[dup], persons$id[dup])
  
  den <- merge(den, tags, by = "affiliation_id", all.x = TRUE)
  den <- merge(den, persons, by.x = "person_id", by.y = "id", all.x = TRUE)
  
  den <- dplyr::transmute(
    den,
    ID = id,
    NAME = alias,
    AFFILIATION = name,
    ROLE = rolelist$name[match(den$role_id, rolelist$id)],
    TAGS = tagnames,
    SECTOR = sector,
    TYPE = type,
    DESCRIPTION = description,
    CREATED = lubridate::ymd_hms(created_date),
    ARCHIVED = lubridate::ymd_hms(archived_date),
    LAST_CHECKED = lubridate::ymd_hms(last_checked),
    CVR_PERSON = as.integer(cvrid),
    CVR_AFFILIATION = cvr,
    PERSON_ID = person_id,
    AFFILIATION_ID = affiliation_id
  )
  
  return(den)
}


#' eliteDB_cvr
#'
#' Hent et komplet den-objekt fra elitedb_cvr
#' @param conn en åben forbindelse til eliteDB
#' @param user brugernavn
#' @param pass kodeord
#' @param db navn på den database data skal hentes fra. enten "elitedb_cvr" eller "elitedb_bigdata"
#' @encoding utf-8
#' @return Et komplet den-objekt, der både indeholder aktive og arkiverede forbindelser
#' @export
#' @examples
#' den        <- eliteDB_cvr(user, password)
eliteDB_cvr  <- function(conn = NULL, user = NULL, pass = NULL, db = "elitedb_cvr"){
  
  if (is.null(conn)){
    conn <- eliteR::eliteDB_open(user, pass, db)
  }
  
  den <- DBI::dbGetQuery(
    conn,
    "SELECT
    c.cvr, c.enhedsnummer, c.role, c.valgform, c.startdate, c.enddate, 
    p.fullname, p. adresse, p.postnummer, p.kommune, 
    a.hovedbranchenavn, a.hovedbranchekode, a.virksomhedsformnavn, a.virksomhedsformkode, a.livsforloebstart, a.livsforloebslut, a.name, 
    a.postnummer as virk_post, a.kommune as virk_kommune, a.adresse as virk_adresse
    FROM Connections c
    LEFT JOIN Persons p
    ON c.enhedsnummer=p.enhedsnummer
    LEFT JOIN Affiliations a
    ON c.cvr=a.cvr"
  )
  
  persons <- DBI::dbGetQuery(conn, "SELECT p.enhedsnummer, p.fullname as alias FROM Persons p")
  
  
  persons$alias <- gsub("\\d", "", x = persons$alias) %>% stringr::str_trim()
  dup <- duplicated(persons$alias) | duplicated(persons$alias, fromLast = TRUE)
  persons$alias[dup] <- paste(persons$alias[dup], persons$enhedsnummer[dup])
  
  den <- merge(den, persons, by.x = "enhedsnummer", by.y = "enhedsnummer", all.x = TRUE)
  
  den <- dplyr::transmute(
    den,
    ENHEDSNUMMER = enhedsnummer,
    CVR = as.integer(cvr),
    NAME = alias,
    AFFILIATION = name,
    ROLE = role,
    VALGFORM = valgform,
    PERSON_START = lubridate::ymd(startdate),
    PERSON_END = lubridate::ymd(enddate),
    AFFIL_START = lubridate::ymd(livsforloebstart),
    AFFIL_END = lubridate::ymd(livsforloebslut),
    PERSON_ADDRESS = adresse,
    PERSON_POST = postnummer,
    PERSON_KOMMUNE = kommune,
    AFFIL_ADRESS = virk_adresse,
    AFFIL_POST = virk_post,
    AFFIL_KOMUNNE = virk_kommune,
    AFFIL_BRANCHE = hovedbranchenavn,
    AFFIL_BRANCHEKODE = hovedbranchekode,
    AFFIL_FORM = virksomhedsformnavn,
    AFFIL_FORMKODE = virksomhedsformkode,
    LABEL = fullname
  )
  
  return(den)
}


# eliteDB_tag_rapport -----------------------------------------------------

#' eliteDB_tag_rapport
#'
#' Få et overblik over tags i eliteDB. Funktionen returnerer en data.frame,
#'    der viser hvor mange affiliations, aktive forbindelser og arkiverede forbindelser, 
#'    der er knyttet til et tag.
#' @param conn en åben forbindelse til eliteDB
#' @encoding utf-8
#' @export
#'
#' @examples
#' conn <- eliteDB_open(user, pass)
#' tags <- eliteDB_tag_rapport(conn)
eliteDB_tag_rapport <- function(conn){
  
  affiliations <- DBI::dbGetQuery(
    conn,
    "SELECT tl.id, COUNT(t.affiliation_id) AS matched_affiliations  
    FROM Tags t
    LEFT JOIN Taglist tl
    ON t.tag_id=tl.id
    GROUP BY t.tag_id
    ORDER BY tl.tag"
  )
  
  active.connections <- DBI::dbGetQuery(
    conn,
    "SELECT tl.id, tl.tag, COUNT(c.affiliation_id) AS matched_positions
    FROM Tags t
    LEFT JOIN Connections c
    ON t.affiliation_id=c.affiliation_id
    LEFT JOIN Taglist tl
    ON tl.id=t.tag_id
    WHERE c.archived_date IS NULL
    GROUP BY t.tag_id
    ORDER BY tl.tag"
  )
  
  archived.connections <- DBI::dbGetQuery(
    conn,
    "SELECT tl.id, tl.tag, COUNT(c.affiliation_id) AS matched_positions_archived
    FROM Tags t
    LEFT JOIN Connections c
    ON t.affiliation_id=c.affiliation_id
    LEFT JOIN Taglist tl
    ON tl.id=t.tag_id
    WHERE c.archived_date IS NOT NULL
    GROUP BY t.tag_id
    ORDER BY tl.tag"
  )
  tags <- merge(active.connections, affiliations, by = "id", all = TRUE)
  tags <- merge(archived.connections, tags, by = "id", all = TRUE)
  
  tags$tag.x[is.na(tags$tag.x)] <- tags$tag.y[is.na(tags$tag.x)]
  
  tags <- dplyr::select(tags,
                id,
                tag = tag.x,
                matched_affiliations,
                matched_positions,
                matched_positions_archived)
  
  return(tags)
}


