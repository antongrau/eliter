# 
# Skip to content
# This repository
# 
# Pull requests
# Issues
# Gist
# 
# @antongrau
# 
# 3
# 1
# 
# 0
# 
# socialfabric/eliteR Private
# Code
# Issues 0
# Pull requests 0
# Projects 0
# Wiki
# Pulse
# Graphs
# eliteR/R/eliteDB.R
# 9805b94 on Mar 28
# @Taarnborg Taarnborg fix encoding problem i eliteDB funktionerne + tilføj 3 data wrangling…
# 189 lines (168 sloc) 5.87 KB
# ## Scriptet indeholder en række funktioner til at tilgå data i eliteDB
# ## Pt 5 funktioner: eliteDB_open, eliteDB_close, eliteDB_read_table, eliteDB_list_tables, eliteDB_den
# 
# # eliteDB_open ------------------------------------------------------------
# 
# #' eliteDB_open
# #'
# #' Opret forbindelse til eliteDB
# #' @param user brugernavn
# #' @param pass kodeord
# #' @param set_names TRUE/FALSE se details
# #' @details Vi oplevede nogle problemer med encoding på forskellige maskiner/styresystemer.
# #'     På Ronnies Windows maskine virker det bedst med set_names = FALSE, mens det på Tobiases
# #'     Mac computer virker bedst med set_names = TRUE. Prøv dig frem for at se hvad der virker bedst i dit tilfælde.
# #' @encoding utf-8
# #' @return en aktiv MySQL forbindelse
# #' @seealso 
# #'     \code{\link{eliteDB_close}} 
# #'     \code{\link{eliteDB_list_tables}}
# #'     \code{\link{eliteDB_read_table}}
# #' @encoding utf-8
# #' @export
# #' @examples
# #' conn <- eliteDB_open(user, pass)
# #' eliteDB_list_tables(conn)
# #' persons <- eliteDB_read_table(conn,"Affiliations_employees")
# #' eliteDB_close(conn)
# eliteDB_open <- function(user, pass, set_names = TRUE){
#   
#   connection <-  DBI::dbConnect(RMySQL::MySQL(), 
#                                 username = user, 
#                                 password = pass,
#                                 host = "www.ogtal.dk", 
#                                 port = 3306, 
#                                 dbname = "elitedb_bigdata"
#   )
#   if (set_names){
#     DBI::dbGetQuery(connection, 'SET NAMES utf8;')
#   }
#   return(connection)
# } 
# 
# # eliteDB_close -----------------------------------------------------------
# 
# #' eliteDB_close
# #'
# #' Luk en aktiv forbindelse til eliteDB
# #' @param conn en åben forbindelse til eliteDB
# #' @seealso 
# #'     \code{\link{eliteDB_open}} 
# #'     \code{\link{eliteDB_list_tables}}
# #'      \code{\link{eliteDB_read_table}}
# #' @encoding utf-8
# #' @export
# #' @examples
# #' conn <- eliteDB_open(user, pass)
# #' eliteDB_list_tables(conn)
# #' persons <- eliteDB_read_table(conn,"Persons")
# #' eliteDB_close(conn)
# eliteDB_close <- function(conn){
#   RMySQL::dbDisconnect(conn)
# } 
# 
# # eliteDB_read_table ------------------------------------------------------
# 
# #' eliteDB_read_table
# #'
# #' Hent data fra eliteDB
# #' @param conn en åben forbindelse til eliteDB
# #' @param table navn på den tabel der skal hentes.
# #' @encoding utf-8
# #' @export
# #' @seealso 
# #'     \code{\link{eliteDB_open}} 
# #'     \code{\link{eliteDB_close}}
# #'     \code{\link{eliteDB_list_tables}}
# #' @encoding utf-8
# #' @export
# #' @examples
# #' conn <- eliteDB_open(user, pass)
# #' eliteDB_list_tables(conn)
# #' persons <- eliteDB_read_table(conn,"Persons")
# #' eliteDB_close(conn)
# eliteDB_read_table <- function(conn, table){
#   result <- RMySQL::dbReadTable(conn, table)
#   return(result)
# }
# 
# # eliteDB_list_tables -----------------------------------------------------
# 
# #' eliteDB_list_tables
# #'
# #' Se hvilke tabeler der er tilgængelige i eliteDB
# #' @param conn en åben forbindelse til eliteDB
# #' @encoding utf-8
# #' @seealso 
# #'     \code{\link{eliteDB_open}} 
# #'     \code{\link{eliteDB_close}} 
# #'     \code{\link{eliteDB_read_table}}
# #' @encoding utf-8
# #' @export
# #' @examples
# #' conn <- eliteDB_open(user, pass)
# #' eliteDB_list_tables(conn)
# #' persons <- eliteDB_read_table(conn,"Persons")
# #' eliteDB_close(conn)
# eliteDB_list_tables <- function(conn){
#   result <- DBI::dbListTables(conn)
#   return(result)
# }
# 
# # eliteDB_den -------------------------------------------------------------
# 
# #' eliteDB_den
# #'
# #' Hent et komplet den-objekt fra eliteDB
# #' @param user brugernavn
# #' @param pass kodeord
# #' @param set_names TRUE/FALSE se details
# #' @details Vi oplevede nogle problemer med encoding på forskellige maskiner/styresystemer.
# #'     På Ronnies Windows maskine virker det bedst med set_names = FALSE, mens det på Tobiases
# #'     Mac computer virker bedst med set_names = TRUE. Prøv dig frem for at se hvad der virker bedst i dit tilfælde.
# #' @encoding utf-8
# #' @return Et komplet den-objekt, der både indeholder aktive og arkiverede forbindelser
# #' @importFrom magrittr "%>%"
# #' @export
# #' @examples
# #' den        <- eliteDB_den(user, password)
# #' den.active <- den[is.na(den$ARCHIVED),]
# eliteDB_den <- function(user, pass, set_names = TRUE) {
#   
#   conn <- eliteDB_open(user, pass, set_names)
#   
#   den <- DBI::dbGetQuery(
#     conn,
#     "SELECT
#     c.id, c.person_id, c.affiliation_id, c.role_id, c.description, c.created_date, c.archived_date, p.cvrid, a.name, a.type, a.cvr, a.sector, a.last_checked
#     FROM Connections c
#     LEFT JOIN Persons p
#     ON c.person_id=p.id
#     LEFT JOIN Affiliations a
#     ON c.affiliation_id=a.id"
#   )
#   
#   tags <- DBI::dbGetQuery(
#     conn,
#     "SELECT
#     t.affiliation_id, GROUP_CONCAT(tl.tag) AS tagnames
#     FROM Tags t
#     LEFT JOIN Taglist tl
#     ON t.tag_id=tl.id
#     GROUP BY t.affiliation_id"
#   )
#   
#   rolelist <- DBI::dbGetQuery(conn, "SELECT * FROM Rolelist")
#   
#   persons <- DBI::dbGetQuery(conn, "SELECT p.id, p.alias FROM Persons p")
#   
#   # Provide all non-unique aliases with an id sufix
#   persons$alias <- gsub("\\d", "", x = persons$alias) %>% stringr::str_trim()
#   dup <- duplicated(persons$alias) | duplicated(persons$alias, fromLast = TRUE)
#   persons$alias[dup] <- paste(persons$alias[dup], persons$id[dup])
#   
#   den <- merge(den, tags, by = "affiliation_id", all.x = TRUE)
#   den <- merge(den, persons, by.x = "person_id", by.y = "id", all.x = TRUE)
#   
#   den <- dplyr::transmute(
#     den,
#     id = id,
#     NAME = as.factor(alias),
#     AFFILIATION = as.factor(name),
#     ROLE = as.factor(rolelist$name[match(den$role_id, rolelist$id)]),
#     TAGS = tagnames,
#     SECTOR = as.factor(sector),
#     TYPE = as.factor(type),
#     DESCRIPTION = description,
#     CREATED = lubridate::ymd_hms(created_date),
#     ARCHIVED = lubridate::ymd_hms(archived_date),
#     LAST_CHECKED = lubridate::ymd_hms(last_checked),
#     CVR_PERSON = as.integer(cvrid),
#     CVR_AFFILIATION = cvr,
#     PERSON_ID = person_id,
#     AFFILIATION_ID = affiliation_id
#   )
#   
#   eliteDB_close(conn)
#   
#   return(den)
# }