library(eliter)

# Den har problemer med filialer hvor den vælger filialen istedetfor moderselskabet

key.words.comp <- c("company","bank", "commercial", "corporat", "compan", "establishments", "business", "manufacturer", "stock exchange", "firm")
key.words.part <- c("university", "universities", "research", "institution", "scientific", "science", "corporation", "company", "municipality", "commune", "academy")
blacklist.part <- c("is a list of",
                    "may also refer to",
                    "commonly refers to",
                    "may refer to", "article disambiguation")

x              <- c("Roskilde Universitet", "RUC", "Roskilde universitets center", "Roskilde University", "Roskille University", "Roskilde University (RUC)", "Facebook", "Roskilde", "Roskilde Festival", "Walt Disney", "UNIVERSIDAD COMPLUTENSE DE MADRID (UMC)")
test           <- match_and_write_chunks_from_wiki(x, file = "dev/w.Rda", key.words = key.words.part, blacklist = blacklist.part)
match <- x 

blacklist <- blacklist.part

search_and_match_to_wiki(tail(match, 10), key.words = key.words.part, blacklist = blacklist.part)

timeout.seconds = 30
save_html = FALSE
wiki.language = "en"
check.names = FALSE

key.words = key.words.part

match_to_wikidata(x = "Aena", key.words = "company") # Has no English wiki-page but is the correct wikidata hit
match_to_wikidata(x = "ælaksjdflk", key.words = "company") # No hit
match_to_wikidata(x = "AES", key.words = key.words.comp) # There is a wikidata site - but it doesn't find it
match_to_wikidata(x = "Johnson & Johnson", key.words = key.words.comp) # The & throws it off

match_to_wikidata(x = "Terma", key.words = key.words.comp)
match_to_wikidata(x = "En+ Group Plc Sponsored GDR RegS", key.words = key.words.comp)
match_to_wikipedia(x = "FORSKNINGSRÅDET FÖR MILJÖ, AREELLA NÄRINGAR OCH SAMHÄLLSBYGGANDE")
match_to_wikidata(x = "AT&T", key.words = key.words.comp, check.names = TRUE)
match_to_wikidata(x = "Johnson & Johnson", key.words = key.words.comp, check.names = TRUE)
match_to_wikidata(x = "McKinsey & Co.", key.words = key.words.comp, check.names = TRUE, valid.qids = "Q310207")
match_to_wikidata(x = "McKinsey & Company, Inc.", key.words = key.words.comp, check.names = TRUE, valid.qids = "Q310207")

match_to_wikidata(x = "H&M - Hennes & Mauritz", key.words = key.words.comp, check.names = TRUE)
match_to_wikidata(x = "H&M  Hennes & Mauritz", key.words = key.words.comp, check.names = TRUE)


match_to_wikidata(x = "SZENT ISTVAN UNIVERSITY", key.words = key.words.part, check.names = TRUE)



n <- match_to_wikidata("UNIVERSIDAD+OR+COMPLUTENSE+OR+DE+OR+MADRID+OR+(UMC)", key.words = key.words.part, check.names = TRUE)

n$search_url

match_to_wikidata(x = , key.words = key.words.comp)

match_to_wikipedia(x = "Roskille University") # No hit
match_to_wikipedia(x = "Johnson & Johnson", check.names = TRUE) # The & throws it off
test           <- match_and_write_chunks_from_wiki(c("AES", "ABB", "Association Recherches, Observations, Formations, Enseignements" ), file = "", key.words = key.words.comp, blacklist = blacklist.part)

search_and_match_to_wiki("nifasdfasdf", "nasdfasdfaf", "kasdfdsf")


# valid QIDs


# Third way -----




x <- "UNIVERSIDAD COMPLUTENSE DE MADRID (UMC)"
match_to_wikidata_special_search(x, key.words = key.words.part)
match_to_wikidata_special_search("Roskilde University (RUC)", key.words = key.words.part)
match_to_wikidata("Roskilde University (RUC)", key.words = key.words.part)
match_to_wikidata_special_search("Mckinsey & Co", key.words = key.words.part)
match_to_wikidata_special_search("Mckinsey & Co (Also Mcksinsey and Company)", key.words = key.words.part)
match_to_wikidata_special_search("Johnson & Johnson", key.words = "multinational")

match_to_wikidata_special_search("SOFIISKI UNIVERSITET SVETI KLIMENT OHRIDSKI", key.words = key.words.part)


# Get wikidata qid for each wikipedia entry ----

x <- c("nonsesssss","Maersk", "ABB_Group", "Acciona", "Adani_Group", "African_Rainbow_Minerals", "Agility_Logistics",
  "Airbus", "Amr_Al-Dabbagh", "Alghanim_Industries", "American_Tower", "AngloGold_Ashanti", "Anheuser-Busch_InBev")




get_wikidata_qid_from_wikipedia_title(x)


# På dansk ------
match <- c("metropolitanskolen", "statens kunstfond", "industrirådet", "københavns universitet", "kbh uni", "mærsk", "A. P. Møller Mærsk")
x <- match

nif <- match_and_write_chunks_from_wiki(x, file = "dev/w.Rda", language = "da", key.words = c("Skole", "school", "Corporation", "university", "universitet"))
View(nif)
