#' Danish Power Elite Careers
#' 
#' A set of sequences 
#' @name careers
#' @doctype data
NULL


#' Danish Elite Network
#' 
#' A affiliation network
#'
#' @name den
#' @docType data
NULL


#' Power Elite 2013
#' 
#' A propopographic dataset on the Danish power elite in 2013
#'
#' @name pe13
#' @docType data
NULL

#'Directors 2008 dataset
#'
#'Prosopographical data on the top 100 CEO's from the 82 largest Danish 
#'corporations in 2008.
#'@details The directors dataset is prosopographical data collected from a wide 
#'  array of sources on biographic and corporate information. Sources include 
#'  the Danish variant of Who's Who (Blaa Bog), a private business information 
#'  database (Greens Erhvervsinformation), journalistic portrait articles, 
#'  article search engines, bibliographic databases and financial reports. CEOs 
#'  from 82 corporations were selected according to their position as CEO in 
#'  December 2007. 18 executives are included on other criteria, taking into 
#'  account the magnitude of the corporations and issues regarding ownership and
#'  control, resulting in a final population of 100 CEOs. The 82 corporations 
#'  have formal ownership and management located in Denmark and were selected 
#'  through either financial capital, measured as having a turnover of over five
#'  billion DKK (650 million Eur.), or organizational capital, defined as having
#'  at least 5000 employees; 34 corporations were included on both criteria, 45 
#'  on financial capital and three on organizational capital alone. To avoid 
#'  including investors, rather than executives, a minimum of 500 employees was 
#'  also required, excluding 12 firms. Companies acting only as subsidiaries 
#'  were also excluded. Data is for public use  and no author permission is 
#'  needed, but we would love to hear from you if you find the data useful. 
#'  You can find an analysis of this dataset in the \link{soc.ca} package.
#'  
#'@name directors08
#'@docType data
#'@author Christoph Ellersgaard
#'@author Anton Grau Larsen
#'@references Ellersgaard, Christoph, Anton Grau Larsen, og Martin D. Munk. 
#'  2012. "A Very Economic Elite: The Case of the Danish Top CEOs". Sociology.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2010. 
#'  "Firmaets Maend". Master Thesis, Copenhagen: University of Copenhagen.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2011. 
#'  "Kulturel kapital blandt topdirektoerer i Danmark - En domineret 
#'  kapitalform?" Dansk Sociologi 22(3):9-29.
#'@references Larsen, Anton Grau, og Christoph Houman Ellersgaard. 2012. "Status
#'  og integration paa magtens felt for danske topdirektoerer". Praktiske 
#'  Grunde. Nordisk tidsskrift for kultur- og samfundsvidenskab 2012(2-3).
#'@keywords data
NULL

#' Top 1000 corporations in Denmark in 2013
#' 
#' This dataset was used for the article: "Who listens to the top? Integration of the largest corporations across sectoral networks" published in Acta Sociologica and written by Anton Grau Larsen and Christoph Ellersgaard in 2017.
#' The example is the code that replicates the analysis in that article.
#' @name corp13
#' @docType data
#' @examples 
#' # Who listens to the top -----
#' library(eliter)
#' library(Matrix)
#' library(RColorBrewer)
#' library(ggthemes)
#' library(car)
#' 
#' data(corp13)
#' 
#' # Active variables ------
#' active <-  data.frame(Turnover = rank(corp13$OMSÆTNING.10, na.last = "keep"),
#' Employees = rank(corp13$ANSATTE.10, na.last = "keep"),
#'                       Equity = rank(corp13$EGENKAPITAL.10, na.last = "keep"),
#'                       Dominating = corp13$dominerende_num,
#'                       Finance = corp13$finance,
#'                       Coop = corp13$coop,
#'                       Global500 = corp13$Global,
#'                       Books = corp13$books,
#'                       "Radio and Tv" = corp13$radio_tv
#' )
#' 
#' rownames(active)    <- corp13$ORG_NAVN
#' colSums(sapply(active, is.na))
#' active.complete     <- na.omit(active)
#' 
#' num.mem <- corp13[, grep("Memberships",colnames(corp13))]
#' 
#' 
#' # Table 2: Network statistics for each sectorial network ------
#' # § A table, divided by sector, with number of affiliations, individuals, largest component, diameter of the largest component
#' 
#' 
#' describe.sector <- function(x){
#'   out                                <- list()
#'   out$"Affiliations"       <- length(table(droplevels(x$AFFILIATION)))
#'   out$"Postions"           <- nrow(x)
#'   out$"Individuals"        <- length(table(droplevels(x$NAME)))
#'   net.sector                         <- eliter::elite.network(droplevels(x), result = "affil")
#'   net.sector.com                     <- largest.component(net.sector, cut.off = 0.001)
#'   out$"Largest component"            <- vcount(net.sector.com)
#'   out$"Component diameter"           <- diameter(net.sector.com, weights = NA, directed = FALSE)
#'   out$"Highest degree"               <- max(degree(net.sector, loops = FALSE, mode = "out"))
#'   out
#' }
#' 
#' table.2 <- t(sapply(sector.rels, describe.sector))
#' 
#' #write.csv(table.2, file = "output_corporate/Table_2_Network_stats_per_sector.csv")
#' 
#' # Table 3: Mean number of memberships per sector -----
#' table.3.list                   <- list()
#' table.3.list$Finance           <- aggregate(num.mem, by = list(corp13$finance), mean, na.rm = TRUE)[2, -1]
#' table.3.list$Dominant          <- aggregate(num.mem, by = list(corp13$dominerende_num), mean, na.rm = TRUE)[2, -1]
#' table.3.list$Coop              <- aggregate(num.mem, by = list(corp13$coop), mean, na.rm = TRUE)[2, -1]
#' table.3.list$"Global 500"      <- aggregate(num.mem, by = list(corp13$Global), mean, na.rm = TRUE)[2, -1]
#' table.3.list$"All"             <- apply(num.mem, 2, mean, na.rm = TRUE)
#' 
#' table.3b.list                   <- list()
#' table.3b.list$Finance           <- aggregate(num.mem, by = list(finance), sd, na.rm = TRUE)[2, -1]
#' table.3b.list$Dominant          <- aggregate(num.mem, by = list(corp13$dominerende_num), sd, na.rm = TRUE)[2, -1]
#' table.3b.list$Coop              <- aggregate(num.mem, by = list(corp13$coop), sd, na.rm = TRUE)[2, -1]
#' table.3b.list$"Global 500"      <- aggregate(num.mem, by = list(corp13$Global), sd, na.rm = TRUE)[2, -1]
#' table.3b.list$"All"             <- apply(num.mem, 2, sd, na.rm = TRUE)
#' 
#' table.3b                        <- t(do.call(what = rbind, args = table.3b.list))
#' table.3b                        <- round(table.3b, 1)
#' rownames(table.3b)              <- sapply(str_split(rownames(table.3b), " : "), tail, 1)
#' 
#' tab.3.n  <- c(sum(corp13$finance, na.rm = TRUE),
#'               sum(corp13$dominerende_num),
#'               sum(corp13$coop),
#'               sum(corp13$Global),
#'               nrow(num.mem)
#' )
#' 
#' table.3                        <- t(do.call(what = rbind, args = table.3.list))
#' table.3                        <- round(table.3, 1)
#' rownames(table.3)              <- sapply(str_split(rownames(table.3), " : "), tail, 1)
#' 
#' table.3[] <- paste(table.3[], " (", table.3b[], ")", sep = "")
#' table.3                        <- rbind(table.3, N = tab.3.n)
#' 
#' #write.csv(table.3, file = "output_corporate/Table_3_Mean_number_of_memberships_per_sector.csv")
#' 
#' # Numbers for big linkers paragraph ----
#' net.corp.all                   <- elite.network(sector.rels$Corporations)
#' net.corp.com                   <- largest.component(net.corp.all, cut.off = 0)
#' table(V(net.corp.com)$memberships)
#' connectors                     <- V(net.corp.com)$name[V(net.corp.com)$memberships > 1]
#' 
#' chairmen                       <- unique(sector.rels$Corporation$NAME[sector.rels$Corporation$ROLE == "Chairman"])
#' ceo                            <- unique(sector.rels$Corporation$NAME[sector.rels$Corporation$ROLE == "Chief executive"])
#' nonordinary                    <- unique(sector.rels$Corporation$NAME[sector.rels$Corporation$ROLE %in% c("Chief executive", "Chairman", "Vice chairman", "Executive")])
#' length(connectors)
#' sum(connectors %in% chairmen)
#' sum(connectors %in% ceo)
#' sum(connectors %in% nonordinary)
#' ordinary                      <- connectors[(connectors %in% nonordinary) == FALSE]
#' length(ordinary)
#' 
#' chairmen.all                  <- V(net.corp.all)$name %in% chairmen
#' ceo.all                       <- V(net.corp.all)$name %in% ceo
#' ordinary.all                  <- (V(net.corp.all)$name %in% nonordinary) == FALSE
#' table(ordinary.all)
#' table(chairmen.all)
#' 
#' rels.corp                     <- sector.rels$Corporation
#' rels.corp                     <- rels.corp[-which((rels.corp$NAME %in% nonordinary) == FALSE),]
#' net.corp.org.red              <- elite.network(rels.corp, result = "affil")
#' net.corp.org.red              <- largest.component(net.corp.org.red, cut.off = 0)
#' net.com.org                   <- elite.network(sector.rels$Corporations, result = "affil")
#' net.com.org                   <- largest.component(net.com.org, cut.off = 0)
#' vcount(net.corp.org.red) - vcount(net.com.org)
#' 
#' # Correlation lines -----
#' changes             <- list()
#' changes$vline       <- geom_vline(xintercept = nrow(corp13) - 250, linetype = "dashed", color = "grey30")
#' changes$ylab        <- ylab("Memberships")
#' 
#' d.p                 <- data.frame(num.mem, omsætning = rank(corp13$OMSÆTNING, na.last = "keep"), navn = corp13$ORG_NAVN, check.names = FALSE)
#' mdp                 <- melt(d.p, id.vars = c("navn", "omsætning"))
#' p.cor.line.turnover <- ggplot(data = mdp, aes(x = omsætning, y = value)) + geom_point(shape = 21, alpha = 0.3, size = 1.2, fill = "whitesmoke") + facet_wrap(~variable, scales = "free_y") + geom_smooth(color = "red2", method = "loess") + theme_bw()
#' p.cor.line.turnover <- p.cor.line.turnover + changes + xlab("Rank by turnover")
#' p.cor.line.turnover <- p.cor.line.turnover + theme_tufte() + theme(strip.text = element_text(size = 12))
#' 
#' # pdf(file = "output_corporate/Figure_2_Correlation_smooths_by_sector.pdf", height = 10, width = 12)
#' # p.cor.line.turnover
#' # dev.off()
#' 
#' # Regression -----
#' reg                <- as.matrix(active)
#' num.mem            <- as.matrix(num.mem)
#' models.memberships <- lm(num.mem ~ reg)
#' 
#' d        <- data.frame(num.mem, reg)
#' 
#' m1       <- lm(Memberships...Corporations ~ Turnover + Employees + Equity + Dominating + Finance + Coop + Global500 + Books + Radio.and.Tv, data = d)
#' vif.val  <- car::vif(m1)
#' 
#' reg.stats <- coef(summary(models.memberships))
#' for (i in seq_along(reg.stats)) colnames(reg.stats[[i]]) <- paste(names(reg.stats)[i],colnames(reg.stats[[i]]))
#' 
#' sum.mod      <- summary(models.memberships)
#' r.squared    <- vector(length = length(sum.mod))
#' for (i in seq_along(sum.mod)) r.squared[i] <- sum.mod[[i]]$adj.r.squared
#' r.squared    <- round(r.squared, 2)
#' 
#' reg.stats <- do.call(cbind, reg.stats)
#' reg.estimate <- reg.stats[, grep("Estimate", colnames(reg.stats))]
#' reg.pvalue   <- reg.stats[, grep("Pr(>|t|)", colnames(reg.stats))]
#' reg.error    <- reg.stats[, grep("Error", colnames(reg.stats))]
#' reg.stars    <- apply(reg.pvalue, 2, symnum, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
#' 
#' est.min <- format(round(reg.estimate - reg.error, 4), scientific = FALSE)
#' est.max <- format(round(reg.estimate + reg.error, 4), scientific = FALSE)
#' est.error <- paste("[", est.min, ":", est.max, "]", sep = "")
#' est.error <- format(round(reg.error, 5))
#' 
#' reg.res      <- reg.estimate
#' reg.res[]    <- paste(format(round(reg.estimate, 5), scientific = FALSE), " (", reg.stars, ")", est.error, sep = "")
#' 
#' reg.res      <- rbind(reg.res, R.squared = r.squared)
#' reg.range    <- apply(reg, 2, range, na.rm = TRUE)
#' reg.range    <- paste("[", reg.range[1,], ":", reg.range[2,],"]", sep = "")
#' colnames(reg.res) <- names(sector.rels)
#' 
#' reg.res      <- cbind(Range = c("", reg.range, ""), Vif = c("", round(vif.val, 2), ""), reg.res)
#' reg.res      <- rbind(reg.res,  mean = c("", "", round(apply(num.mem, 2, mean), 2)))
#' reg.res      <- rbind(reg.res,  max = c("", "", round(apply(num.mem, 2, max), 2)))
#' reg.res
#' 
#' #write.csv(reg.res, file = "output_corporate/table.4_Regressions.csv")
#' 
#' ###############################################################################
#' # Figure 1 - The corporate network
#' market.leader         <- corp13$dominerende
#' levels(market.leader) <- c("Incumbent", "Dominated")
#' 
#' net.corp     <- elite.network(sector.rels$Corporations, result = "affil")
#' lay          <- layout_with_fr(net.corp, weights = E(net.corp)$weight^2, grid = "nogrid") * -1
#' p            <- graph.plot(net.corp, lay, vertex.fill = market.leader, vertex.size = num.mem[,9], edge.alpha = 0.1, edge.color = "black", edge.size = 0.5)
#' p            <- p + scale_fill_manual(values = c("red2", "white"), name = "Market position") + scale_size_continuous(range = c(1, 6), name = "Non-corporate \nmemberships")
#' p
#' 
#' # pdf(file = "output_corporate/Figure.1_corporate_network.pdf", height = 11, width = 12)
#' # p
#' # dev.off()
#' 
#' # Appendix -----
#' deg      <- degree(net.corp)
#' com      <- V(net.corp)$name %in% V(net.com.org)$name
#' turn     <- corp13$OMSÆTNING.10
#' mem      <- num.mem[,9]
#' com      <- factor(com, labels = c("No", "Yes"))
#' 
#' rd       <- data.frame(Name = V(net.corp)$name, "Turnover" = turn, "Non-corporate memberships" = mem, "Degree" = deg, "In component" = com, check.names = FALSE)
#' rd.com   <- rd[com == "Yes",]
#' rd.com   <- rd.com[order(rd.com$Turnover, decreasing = TRUE),]
#' 
#' rd.notcom   <- rd[com == "No",]
#' rd.notcom   <- rd.notcom[order(rd.notcom$Turnover, decreasing = TRUE),]
#' 
#' # write.csv(head(rd.com, 25), file = "output_corporate/Appendix table_com.csv", row.names = FALSE)
#' # write.csv(head(rd.notcom, 25), file = "output_corporate/Appendix table_notcom.csv", row.names = FALSE)
NULL

# load("~/My Dropbox/R/Corporate/output_corporate/corporate_data_all.Rda")
# colnames(total.data)
# corp13  <- data.frame(
#            Name                      = total.data$ORG_NAVN,
#            CVR                       = total.data$CVR_NR,
#            Adress                    = total.data$ADRESSE,
# 
#            Sector.Børsen             = total.data$BØRSEN.BRANCHE,
#            Turnover                  = total.data$OMSÆTNING.10,
#            Turnover.change.09.10     = total.data$ÆNDRING.OMSÆTNING.9.10,
#            Result.before.taxes       = total.data$RESULTAT.FØR.SKAT.10,
#            Result                    = total.data$NETTORESULTAT.10,
#            Balance                   = total.data$BALANCE.10,
#            Equity                    = total.data$EGENKAPITAL.10,
#            Employees                 = total.data$ANSATTE.10,
#            Employees.change.09.10    = total.data$ÆNDRING.ANSATTE.9.10,
#            
#            Component                 = total.data$component,
#            Degree                    = total.data$deg,
#            Betweenness               = total.data$between,
#            Closeness                 = total.data$close,
#            Reach                     = total.data$n2,
#            
#            Memberships.corporations    = total.data$Memberships...Corporations,
#            Memberships.business.organisations = total.data$Memberships...Business.organisations,
#            Memberships.interest.groups = total.data$Memberships...Interest.groups,
#            Memberships.state           = total.data$Memberships...State,
#            Memberships.science.and.education = total.data$Memberships...Science.and.education,
#            Memberships.culture         = total.data$Memberships...Culture,
#            Memberships.royal           = total.data$Memberships...Royal,
#            Memberships.leader.networks = total.data$Memberships...Leadership,
#            Memberships.all.noncorporate = total.data$Memberships...All.noncorporate.sectors,
#            
#            All.media           = total.data$all_media,
#            National.newspapers = total.data$national_papers,
#            Regional.newspapers = total.data$regional_papers,
#            Local.newspapers    = total.data$local_papers,
#            Magazines           = total.data$magazines,
#            Radio.TV            = total.data$radio_tv,
#            Webmedia            = total.data$websources,
#            Newsbureaus         = total.data$newsbureau,
#            Books               = total.data$books,
#            
#            Region              = total.data$region,
#            Coop                = total.data$coop,
#            Finance             = total.data$finance,
#            Stockexchange       = total.data$stockexchange,
#            Sector.dominance    = total.data$dominerende,
#            Global500           = total.data$Global
#            )
# 
# save(corp13, file = "~/soc.elite/data/corp13.rda")



##################################################################################
# Generate data 
# source("~/My Dropbox/R/Elite/after_match_2.R")
# 
# rel.all          <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_ALL.csv", sep="|", encoding="UTF-8", stringsAsFactor = FALSE)
# 
# # BIQ LINK
# biq.id           <- rel.all$BIQ_PERSON_ID
# biq.link         <- paste("http://www.biq.dk/people/", biq.id, sep="")
# biq.link[biq.id == 0]    <- ""
# 
# # Essential columns
# # Gender
# load("~/My Dropbox/R/Elite/Navne/names_gender")
# gender.rel        <- find.gender(as.character(rel.all$NAVN), names.gender)
# levels(gender.rel)[2] <- "Undefined"
# 
# # CVR Number
# org.virk.other         <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ_andre_virksomheder_connections.csv", sep="|", encoding="UTF-8")
# org.virk               <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ.csv", sep="|", encoding="UTF-8")
# org.fond               <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ_fonde.csv", sep="|", encoding="UTF-8")
# 
# cvr.virk.other         <- data.frame(ORG_NAVN = org.virk.other$ORG, CVR = org.virk.other$CVR_NR)
# cvr.virk               <- data.frame(ORG_NAVN = org.virk$ORG, CVR = org.virk$CVR_NR)
# cvr.fond               <- data.frame(ORG_NAVN = as.character(org.fond$NAVN), CVR = org.fond$CVR)
# cvr.all                <- rbind(cvr.virk.other, cvr.virk, cvr.fond)
# cvr                    <- vector(length = nrow(rel.all))
# 
# for (i in 1:nrow(cvr.all))  cvr[which(rel.all$ORG_NAVN ==  cvr.all$ORG_NAVN[i])] <- cvr.all$CVR[i]
# 
# kilde            <- as.factor(rel.all$kilde)
# levels(kilde)    <- c("State", "Events", "Parliament", "Foundations", "Commissions", 
#                          "NGO", "State", "Corporations", "VL-networks") # Her er der et ækelt hack der tager nogle grupper ud der havde "" som kilde og angiver dem til stat.
# org              <- rel.all$ORG_NAVN
# 
# # TAGS
# 
# tag.frame <- rel.all[,grep("TAG", colnames(rel.all))]
# tag.frame <- apply(tag.frame, 2,as.character)
# tag.frame[tag.frame == ""] <- NA
# 
# nif          <- as.list(as.data.frame(t(tag.frame)))
# hurma        <- lapply(nif, na.omit)
# tag.label    <- unlist(lapply(hurma, paste, collapse = ", "))
# 
# 
# 
# # Output
# data             <- data.frame(NAME        = rel.all$NAVN_MATCH,
#                                AFFILIATION = rel.all$ORG_NAVN,
#                                ROLE        = rel.all$ROLE,
#                                GENDER      = gender.rel,
#                                DESCRIPTION = rel.all$BESKRIVELSE,
#                                SOURCE      = kilde,
#                                BIQ_LINK    = biq.link,
#                                CVR         = cvr,
#                                TAGS        = tag.label     
#                                )
# 
# 
# # Translated version
# # Export
# den                         <- data
# save(den, file = "~/soc.elite/data/den.rda")
# 

###########################################################################
# Power elite 2013 [pe13]
# ind             <- read.csv("~/My Dropbox/Elite/Data/Data/Individuals_elite.csv", sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ".", na.strings = c("", NA))
# load("~/My Dropbox/R/hoved/saved_results")
# 
# ind             <- ind[order(ind$Name),]
# 
# # Check ordering
# all.equal(as.character(ind$Name), V(net.elite)$name)
# 
# # Order of levels in Sector_cat
# oo              <- c(
#   "Business: Top 200",
#   "Business: Multiposition",
#   "Business: Medium-small",
#   "Business: Investment and Pensions",
# 
#   "Interest groups: Employers and business",
#   "Interest groups: Unions",
# 
#   "Interest groups: Farming",
#   "Interest groups: Consumers",
# 
#   "State and politics: Royal court",
#   "State and politics: Politics",
#   "State and politics: Public Officials",
# 
#   "Science and education: University leaders",
#   "Science and education: Economists and political scientists",
#   "Science and education: Other scientists",
#   "Science and education: Education",
# 
#   "Culture and law: Culture and charities" ,
#   "Culture and law: Law"
# )
# 
# ind$Sector_order            <- factor(ind$Sector_order, levels = oo)
# 
# # Rownames
# rownames(ind) <- ind$Name
# 
# net.elite <- upgrade_graph(net.elite)
# 
# # Save
# pe13          <- ind
# 
# # get coordinates 
# 
# load(file = "~/Dropbox/R/politiken/outputs_politiken/SAVE/layout.Rda")
# 
# add_layout_(graph = net.elite, lay[,-1])
# 
#  
# V(net.elite)$layout.x <- lay[,2]
# V(net.elite)$layout.y <- lay[,3]
# 
# net.elite$layout <- cbind(x = lay[, 2], y = lay[, 3])
#  
# save(pe13, net.elite, file = "~/eliter/data/pe13.rda")

##########################################################################
# Names.gender ----
# load("raw_data/names_gender")
# 
# ###############################################################################
# # # Postnumre
# postnumre <- read.csv("raw_data/postnumre.csv", sep = ";", fileEncoding = "UTF-8")
# 
# #
# #
# save(names.gender, postnumre, file = "R/sysdata.rda")
#
###############################################################################
# Firmaets mænd - Directors 2008
# directors08 <- read.csv("~/My Dropbox/Elite/Data/Firmaets Mænd 2008/Data_directors_2008.csv", sep = ",", fileEncoding = "UTF-8", dec = ",")
# save(directors08, file = "~/soc.elite/data/directors08.rda")


