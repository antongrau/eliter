# # Spell graph example -----
# # load("~/Desktop/spell.Rda")
# # library(eliter)
# # samp         <- sample(1:vcount(spell.graph), size = vcount(spell.graph)*0.9)
# # gs            <- delete.vertices(spell.graph, v =  samp)
# 
# prior        <- prior.connections(spell.graph)
# prior.rle    <- lapply(prior, rle)
# 
# prior.duration <- sapply(prior.rle, function(x) x$lengths[1])
# gap.duration   <- sapply(prior.rle, function(x) x$lengths[2])
# number.of.gaps <- sapply(prior.rle, function(x) length(x$lengths))
# 
# x <- prior.rle$`Poul Thorup %--% Verner Bach Pedersen`
# 
# rle.to.data.frame <- function(x){
#   l      <- length(x$lengths)
#   d      <- data.frame(lengths = x$lengths[-l],
#                        values  = x$values[-l],
#                        n.break = rep(seq(1:(l-1)/2), each = 2)
#              ) 
#   d
#   
#   }
# 
# 
# prior.rle.dat <- lapply(prior.rle, rle.to.data.frame)
# 
# 
# 
# cor(prior.duration, gap.duration)
# 
# d              <- data.frame(prior.duration, gap.duration)
# 
# library(ggthemes)
# 
# p              <- ggplot(d, aes(y = gap.duration, x = prior.duration)) + geom_point(size = 0.1) + geom_smooth(color = "red")
# p              <- p + geom_rangeframe() + theme_tufte() 
# p
# 
# # Survival ! ------
# 
# 
# # Datasættet skal have:
# # For alle afsluttede ties!
# # Dvs. en spell.graph hvor alle ties er afsluttede i 2010 - så har de haft 6 år til at genopstå
# # Men vi skal splitte så ties der reemerger flere gange der skal vi have dem delt op!
# # Vi skal bruge:
# # Tie reemergence! TRUE/FALSE
# # Tie duration
# # Break duration - hvis de aldrig genopstår skal den være fra break til 01-01-2016
# 
# reference.month <- spell.graph$reference.month
# reference.month + months(1392)
# reference.month + months(1200)
# 
# gs          <- delete.edges(spell.graph, edges = which(E(spell.graph)$end <= 1392))
# gs          <- delete.edges(spell.graph, edges = which(E(spell.graph)$end <= 1200)) # Her capper vi fra bunden - nok ikke nødvendigt, men det gør vi for computerens skyld.
# 
# 
# 
# 
# # Spell graph - Weighted network
# #library(eliter)
# #load("~/Dropbox/GNA/R/dynacore_data/graph.spell.clean.Rda")
#load("~/Dropbox/GNA/R/dynacore_data/den.clean.Rda")
# #match <- read.csv("~/Dropbox/GNA/Til DST/match_cvr.csv", sep = ",")
# 
# #match$nchar <- nchar(match$ENHEDSNUMMER)
# #
# # View(match[ year(match$PERSON_END) == 2016 & match$nchar == 10,])
# # set <- match[ year(match$PERSON_END) == 2016 & match$nchar == 10,]
# # table(set$PERSON_KOMMUNE)
# #
# # View(set)
# # den.sample <- as.den(den.clean[order(den.clean$NAME),][1:100000,])
# 
# # den.sample  <- as.den(den.clean[,])
# #
# 
# # spell.graph <- graph.from.spells(as.den(den.clean))
# # save(spell.graph, file = "~/Desktop/spell.Rda")
# # spell.graph <- 
# # start <- 1391 - 120
# # end   <- 1391
# # max(E(spell.graph)$start)
# # ref <- spell.graph$reference.month
# # 
# # ref %m+% months(start)
# # ref %m+% months(end)
# # 
# # samp         <- sample(1:vcount(spell.graph), size = vcount(spell.graph)*0.9)
# # gs            <- delete.vertices(spell.graph, v =  samp)
# # 
# # g <- weighted.graph(gs, start, end, to.distance = F)
# # summary(E(g)$weight)
# # 
# # table(E(g)$weight <= 1)
# 
# ##############################################################
# # OLD
# ##############################################################
# # weighted.graph <- function(spell.graph, start, end){
# # 
# #   decay           <- function(x) x * 0.95
# #   #distance.weight <- 
# #   
# # 
# #   del        <- which(E(spell.graph)$start > end | E(spell.graph)$end < start)
# #   g          <- delete.edges(spell.graph, del)
# # 
# #   period       <- start:end
# #   adj.cum      <- get.adjacency(period.graph(g, start = period[1], end = period[1]))
# #   pb           <- txtProgressBar(min = 2, max = length(period), style = 3)
# # 
# #   for (i in 2:length(period)) {
# #     t            <- period[i]
# #     adj.t        <- get.adjacency(period.graph(g, start = t, end = t))
# #     adj.cum      <- adj.cum + adj.t
# #     sv.cum       <- as(adj.cum, Class = "sparseVector")
# #     sv.t         <- as(adj.t, Class = "sparseVector")
# #     set          <- sv.cum@i %in% sv.t@i
# #     sv.cum@x[!set]   <- decay(sv.cum@x[!set])
# #     adj.cum@x        <- sv.cum@x
# #     setTxtProgressBar(pb, i)
# #   }
# #   close(pb)
# # 
# # graph_from_adjacency_matrix(adj.cum, mode = "undirected", weighted = TRUE)
# # }
# # 
# # nif <- wg
# # wg <- nif
# # wg <- weighted.graph(spell.graph, start, end)
# # 
# # save(wg, file = "~/Desktop/wg.Rda")
# # 
# # 
# # E(wg)$weight <- E(wg)$weight/12
# # 
# # dg <- delete.edges(wg, which(E(wg)$weight < 1))
# # 
# # tail(sort(degree(dg.com)), 50)
# # 
# # s <- seq(from = 0.1, to = 20, by = 0.1)
# # plot(cumsum(exp(s)))
# # 
# # plot(besselI(s, nu = 60))
# # 
# # dg.com  <- largest.component(dg)
# # dg.com  <- delete_edge_attr(dg, "weight")
# # 
# # # s <- 1:120
# # # 
# # # x <- rev((1:120)/12)
# # # 
# # # 
# # # sigmoid <- function(x, params =  c(L = 60, k = 0.09, 1)) params[1]/ (1 + exp(1)^(-params[2] * (x - params[3])))
# # # 
# # # 
# # # sigmoid <- function(x, params =  c(L = 120, k = 0.09, 60)) params[1]/ (1 + exp(1)^(-params[2] * (x - params[3])))
# # # 
# # # plot(sigmoid(120:1))
# # # 
# # # u <- 120
# # # sigmoid(u)
# # # for (i in 1:120) u[i+1] <- sigmoid(u[i])  
# # # u
# # # plot(u)
# # # 
# # # 
# # # 
# # # 
# # # 
# # # u <- 12
# # # for (i in 1:120) u[i+1] <- u[i] - sigmoid(-u[i], SoftMax = )
# # # plot(u)
# # # 
# # # 
# # # 
# # # u <- 120
# # # for (i in 1:120) u[i+1] <- u[i] - log(u[i])
# # # plot(u)
# # # 
# # # u <- 120
# # # for (i in 1:120) u[i+1] <- u[i] - log((u[i]), 10)
# # # u
# # # plot(u)
# # # 
# # # log(1:120)
# # # 
# # # x <- 1:120
# # # 
# # # 
# # # 
# # # library(sigmoid)
# # # ?sigmoid_output_to_derivative
# # # 
# # # logistic(x, x0 =0.05, k = 1)
# # # 
# # # sigmoid(111.5136, params)
# # # 
# # dg.com  <- betweenness.decomposition(dg.com)
# # dg.n    <- connect.neighborhood(dg.com, order = 2)
# # core    <- coreness(dg.n)
# # # #sp.com  <- shortest.paths(dg.com)
# # # #core    <- find.core(sp.com, reach = 3)
# # # table(core)
# # # g.core  <- dg.com - which(core != max(core))
# # # tail(sort(degree(g.core)), 20)
# # # 
# # # 
# # # 
# # # #
# # # #
# # # #
# # # #
# # samp         <- sample(1:vcount(spell.graph), size = vcount(spell.graph)*0.8)
# # graph.spell  <- delete.vertices(spell.graph, v =  samp)
# # prior        <- prior.connections(graph.spell)
# # 
# # # Length of break
# # break.length <- sapply(prior, length)
# # plot(table(break.length))
# # prior[which(break.length == 318)]
# # 
# # # Share of edges with a prior break
# # graph.simple         <- simplify(graph.spell)
# # length(prior)/ecount(graph.simple)
# # 
# # # Is consecutive?
# # is.consecutive.break <- function(x) any(var(diff(x)) > 0)
# # cons.break           <- sapply(prior, is.consecutive.break)
# # 
# # # # #
# # # #
# # # # table(cons.break)
# # # #
# # # # rle(diff(x))
# # # # inverse.rle(x)
# # # #
# # # # # Debug
# # # # out[pause.length == 334]
# # # # table(pause.length)
# # # #
# # # #
# # # # # Active in this year
# # # # active.period <- interval(start = "2016-01-01", end = "2016-12-31")
# # # # edge.interval <- interval(start = E(graph.spell)$start, end = E(graph.spell)$end)
# # # # active.edges  <- int_overlaps(edge.interval, active.period)
# # # # graph.active  <- delete.edges(graph.spell, which(active.edges == FALSE))
# # # # graph.now     <- delete.vertices(graph.spell, degree(graph.active) == 0)
# # # #
# # # # prior.now     <- prior.connections(graph.now)
# # # # graph.simple         <- simplify(graph.now)
# # # # length(prior)/ecount(graph.simple)
# # # #
# # # #
# # # #
# # # # simplify(graph.active)
# # # # graph.active
# # # #
# # # # V(graph.spell) %in% V(graph.active)
# # # # summary(graph.now)
# # # # summary(graph.spell)
# # # # summary(graph.active)
# # # # end          <- head_of(graph.active, 1:10)
# # # #
# # # # graph.active[1:10,1:10]
# # # #
# # # # count_multiple(graph.active)
# # # #
# # # 
# # # 
# # # 
# # # #
# # # # # library(Matrix)
# # # # # diag(m) <- 0
# # # # # m
# # # # # drop0(m)
# # # # # m       <- Diagonal(10)
# # # # # m1 <- as(m, "generalMatrix")
# # # # # m1@i <- integer(0)
# # # # # m1@p <- m1@p * 0L
# # # # # m1@x <- numeric(0)
# # # # #
# # # # # m1 <- as(m, "generalMatrix")
# # # # #
# # # # # m2 <- as(m1, "TsparseMatrix")
# # # # #
# # # # # del <- m2@i == m2@j
# # # # # m2@i <- m2@i[!del]
# # # # # m2@j <- m2@j[!del]
# # # # # m2@x <- m2@x[!del]
# # # # #
# # # # #
# # # # # remove.diag <- function(x){
# # # # #   m1 <- as(x, "generalMatrix")
# # # # #   m2 <- as(m1, "TsparseMatrix")
# # # # #   del <- m2@i == m2@j
# # # # #   m2@i <- m2@i[!del]
# # # # #   m2@j <- m2@j[!del]
# # # # #   m2@x <- m2@x[!del]
# # # # #   m2
# # # # #
# # # # # }
# # # # # data(den)
# # # # # m <- adj.ind(den)
# # # # #
# # # # # nif <- remove.diag(m)
# # # # # naf <- m
# # # # # diag(naf) <- 0
# # # # #
# # # # #
# # # # # nif[,1]
# # # # #
# # # # # nif[1:10, 1:10]
# # # # #
# # # # #
# # # # #
# # # # # # Middle levels of power -----
# # # # # # library(eliter)
# # # # # # data(den)
# # # # # # den           <- as.den(den)
# # # # # # graph.elite   <- elite.network(den)
# # # # # # graph.linkers <- largest.component(graph.elite)
# # # # # # graph.linkers <- betweenness.decomposition(graph.linkers)
# # # # # # sp            <- shortest.paths(graph.linkers)
# # # # # # core          <- find.core(sp)
# # # # # #
# # # # # # list.dens     <- tags.to.sectors(den, standard.sectors(sets = "English"))
# # # # # # role          <- sectors.to.role(den, list.dens, mutually.exclusive = TRUE)
# # # # # #
# # # # # # both          <- intersect(names(role), V(graph.linkers)$name)
# # # # # # role          <- role[names(role) %in% both]
# # # # # # in.g          <- V(graph.linkers)$name %in% names(role)
# # # # # # role.graph    <- rep("Not a director", vcount(graph.linkers))
# # # # # # role.graph[in.g] <- as.character(role)
# # # # # #
# # # # # # gd            <- data.frame(role.graph, core)
# # # # # #
# # # # # # p             <- ggplot(gd, aes(x = core, fill = role.graph)) + geom_density()
# # # # # # p + facet_wrap(~role.graph, scales = "free_y") + theme_bw()
# # # # # #
# # # # # # gd            <- gd[order(gd$core, decreasing = TRUE),]
# # # # # # gd
# # # # # #
# # # # # #
# # # # # # # Andel af sektorens direktører der er tilbage på et givent tidspunkt ------
# # # # # # ft            <- table(gd$core, gd$role.graph)
# # # # # # sumd          <- t(t(apply(ft, 2, function(x)cumsum(rev(x)))) / colSums(ft))
# # # # # # md            <- melt(sumd)
# # # # # # p             <- ggplot(md, aes(x = Var1,y = value, color = Var2)) + geom_line()
# # # # # # p             <- p + facet_wrap(~Var2)
# # # # # # p
# # # # # #
# # # # # # # Andel af alle der er i et givent level der er fra en sektor ----
# # # # # # ft            <- t(table(gd$core, gd$role.graph))
# # # # # # f.remain      <- t(rowSums(ft) - t(apply(ft, 1, cumsum)))
# # # # # # fm            <- f.remain / rowSums(f.remain)
# # # # # #
# # # # # # md            <- melt(fm)
# # # # # # p             <- ggplot(md, aes(x = Var1, y = value, color = Var2)) + geom_line()
# # # # # # p             <- p + facet_wrap(~Var2, scales = "free_y")
# # # # # # p + theme_bw()
# # # # # #
# # # # # # # Not mutually exclusive and with chairmen -----
# # # # # # role          <- sectors.to.role(den, list.dens, mutually.exclusive = FALSE, role = c("Chairman", "Chief executive", "Executive", "Vice chairman"))
# # # # # #
# # # # # # both          <- intersect(rownames(role), V(graph.linkers)$name)
# # # # # # role          <- role[rownames(role) %in% both,]
# # # # # # in.g          <- V(graph.linkers)$name %in% rownames(role)
# # # # # # role.graph    <- matrix(0, nrow = vcount(graph.linkers), ncol = ncol(role))
# # # # # # role.graph[in.g,] <- as.numeric(role)
# # # # # #
# # # # # # colnames(role.graph) <- colnames(role)
# # # # # #
# # # # # # gd           <- aggregate(role.graph, by = list(core = core), FUN = sum)[,-1]
# # # # # #
# # # # # # f.remain      <- t(colSums(gd) - t(apply(gd, 2, cumsum)))
# # # # # # fm            <- t(f.remain)
# # # # # # #fm            <- t(f.remain) / colSums(gd)
# # # # # #
# # # # # # md            <- melt(t(fm))
# # # # # # p             <- ggplot(md, aes(x = Var1, y = value, color = Var2)) + geom_line()
# # # # # # p             <- p + facet_wrap(~Var2, scales = "free_y")
# # # # # # p + theme_bw() + xlab("Coreness") + ylab("Number of executives or chairmen")
# # # # #
# # # # #
# # # # #
# # # # #
# # # # # # # # # # DEN summary -----
# # # # # # # # #
# # # # # # # # den <- eliteDB.connections(pass )
# # # # # # # # b <- den
# # # # # # # # load("~/My Dropbox/Elite og Tal/Projekter/Magtelite-projektet 2016/Magtelite R/den.db.Rda")
# # # # # # # # den <- den.db
# # # # # # # # library(qlcMatrix)
# # # # # # # # summary.den <- function(den){
# # # # # # # #   incidence         <- xtabs(data = den, formula = ~ NAME + AFFILIATION, sparse = T)
# # # # # # # #   adj.affil         <- crossprod(incidence)
# # # # # # # #   diag(adj.affil)   <- 0
# # # # # # # #   cs                <- colSums(incidence)
# # # # # # # #   rs                <- rowSums(incidence)
# # # # # # # #
# # # # # # # #
# # # # # # # #
# # # # # # # #   affiliations      <- ncol(incidence)
# # # # # # # #   individuals       <- nrow(incidence)
# # # # # # # #   positions         <- nrow(den)
# # # # # # # #   hangers           <- sum(rs == 1)
# # # # # # # #   isolated.affils   <- sum(rowSums(adj.affil) == 0)
# # # # # # # #
# # # # # # # #   max.members       <- max(cs)
# # # # # # # #   max.memberships   <- max(rs)
# # # # # # # #
# # # # # # # #
# # # # # # # # }
# # # # # # # #
# # # # # # # #
# # # # # # # # # changes       <- function(den){
# # # # # # # # #
# # # # # # # # #   # Number of affiliations
# # # # # # # # #   n.affil      <- table(den.db$SOURCE[duplicated(den.db$AFFILIATION) == FALSE])
# # # # # # # # #   # Number of positions
# # # # # # # # #   n.positions  <- table(den.db$SOURCE)
# # # # # # # # #   # Number of individuals
# # # # # # # # #   n.ind        <- table(den.db$SOURCE[duplicated(den.db$NAME) == FALSE])
# # # # # # # # #   # Number of archived positions
# # # # # # # # #   n.archived   <- table(den.db$SOURCE[is.na(den.db$ARCHIVED)])
# # # # # # # # #   # Number of active positions
# # # # # # # # #   n.active     <- n.positions - n.archived
# # # # # # # # #   # Last change
# # # # # # # # #   recent       <- lapply(split(den.db$MODIFIED, f = den.db$SOURCE), function(x) max(x, na.rm = TRUE))
# # # # # # # # #   recent       <- do.call("c", recent)
# # # # # # # # #
# # # # # # # # #   # Newest affiliation
# # # # # # # # #
# # # # # # # # #   # Percentage of individuals with CVR numbers
# # # # # # # # #
# # # # # # # # #   data.frame("Affiliations"  = as.vector(n.affil),
# # # # # # # # #              "Positions"     = as.vector(n.positions),
# # # # # # # # #              "Individuals"   = as.vector(n.ind),
# # # # # # # # #              "Archived pos." = as.vector(n.archived),
# # # # # # # # #              "Active pos."   = as.vector(n.active),
# # # # # # # # #              "Last change"   = recent)
# # # # # # # # #
# # # # # # # # #
# # # # # # # # #
# # # # # # # # #
# # # # # # # # # }
# # # # # # # # #
# # # # # # # # #
# # # # # # # # #
# # # # # # # # #
# # # # # # # # # # # #####################################################################################
# # # # # # Geo - elite kodning
# # # # #
# # # # # # # # # #
# # # # # # # # # # as.matrix(table(deg.afstand.2km))
# # # # # # # # # # as.matrix(table(deg.afstand.10km))
# # # # # # # # # #
# # # # # # # # # # social.geography <- deg.afstand.2km
# # # # # # # # # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km == 0] <- "Isolated at 10km"
# # # # # # # # # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km != 0] <- "Isolated at 2km"
# # # # # # # # # # social.geography[deg.afstand.2km %in% 1:2]                     <- "1-2"
# # # # # # # # # # social.geography[deg.afstand.2km %in% 3:6]                     <- "3-6"
# # # # # # # # # # social.geography[deg.afstand.2km %in% 7:20]                    <- "7-20"
# # # # # # # # # # social.geography[deg.afstand.2km %in% 21:30]                   <- "21-30"
# # # # # # # # # # social.geography[deg.afstand.2km %in% 31:max(deg.afstand.2km)] <- "+30"
# # # # # # # # # #
# # # # # # # # # # as.matrix(table(social.geography))
# # # # # # # # # #
# # # # # # # # # # graph.plot(graph.afstand, vertex.fill = social.geography, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # # # # # # # #
# # # # # # # # #
# # # # # # # # # # ###############################################
# # # # # # # # # # # REACH ---
# # # # # # # # # #
# # # # # # # # # # # R er hvor langt væk en alter må være fra ego
# # # # # # # # # # # Først sletter vi alle edges med en vægt over R
# # # # # # # # # # # Derefter tager vi et neighborhood med make_ego_graph med en "order" på R/min(E(graph)$weight)
# # # # # # # # # # # Nu tager vi så alle shortests paths for ego graphen og sletter alle over R.
# # # # # # # # # #
# # # # # # # # # # # Vi kan udregne order mere dynamisk tror jeg
# # # # # # # # # # # Vi kan også klare det sidste step mere smart.
# # # # # # # # # library(soc.elite)
# # # # # # # # # library(igraph)
# # # # # # # # # data(den)
# # # # # # # # # data(pe13)
# # # # # # # # # graph <- net.elite
# # # # # # # # # graph.all <- elite.network(droplevels(den))
# # # # # # # # # R <- 2.1
# # # # # # # # # reach <- function(graph, R = 2.1){
# # # # # # # # # g          <- graph
# # # # # # # # # g          <- delete.edges(g, which(E(g)$weight > 2.1))
# # # # # # # # # #order      <- ceiling(R/min(E(g)$weight))
# # # # # # # # # order      <- R
# # # # # # # # #
# # # # # # # # # rr <- function(id, g, order){
# # # # # # # # #   e        <- make_ego_graph(graph = g, order = order, nodes = id)[[1]]
# # # # # # # # #   name     <- which(V(e)$name %in% V(g)$name[id])
# # # # # # # # #   es       <- e[name,]
# # # # # # # # #   min.es   <- order - min(es[es > 0])
# # # # # # # # #   e        <- delete.edges(e, which(E(e)$weight > min.es))
# # # # # # # # #   sum(distances(graph = e, v = name) >= order)
# # # # # # # # # }
# # # # # # # # #
# # # # # # # # # reach      <- laply(1:vcount(g), rr, g = g, order = order)
# # # # # # # # # reach
# # # # # # # # # }
# # # # # # # # #
# # # # # # # # # a <- system.time(reach(graph))
# # # # # # # # # a
# # # # # # # # #
# # # # # # # # # b <- system.time(reach(graph.all))
# # # # # # # # # b
# # # # # # # # #
# # # # # # # # # ?graph.bfs
# # # # # # # # #
# # # # # # # # # order         <- 2
# # # # # # # # # id            <- 1
# # # # # # # # #
# # # # # # # # #
# # # # # # # # # adj           <- cbind(a = c(0, 1.5, 0, 0, 1, 1, 0),
# # # # # # # # #                        b = c(1.5, 0, 0.5, 1, 1, 1, 0),
# # # # # # # # #                        c = c(0, 0.5, 0, 0, 0, 0, 0),
# # # # # # # # #                        d = c(0, 1, 0, 0, 0, 0, 0),
# # # # # # # # #                        e = c(1, 1, 0, 0, 0, 0, 1),
# # # # # # # # #                        f = c(1, 1, 0, 0, 0, 0, 1),
# # # # # # # # #                        g = c(0, 0, 0, 0, 1, 1, 0))
# # # # # # # # # rownames(adj) <- colnames(adj)
# # # # # # # # # adj <- Matrix(adj)
# # # # # # # # #
# # # # # # # # # reach.from.adj <- function(adj, id, order){
# # # # # # # # # #adj[adj == 0] <- NA
# # # # # # # # #
# # # # # # # # # adj           <- adj[,which(adj[id, ] > 0)]
# # # # # # # # # first         <- adj[id,]
# # # # # # # # # ar.i          <- which(adj != 0, arr.ind = T) # Det her kan hives ud af en sparse matrice, hvis det logiske udtryk er langsomt
# # # # # # # # # for(i in 1:length(first)){
# # # # # # # # # ind <- ar.i[ar.i[,2] == i, ]
# # # # # # # # # adj[ind[,1], i]  <- adj[ind[,1], i] + first[i]
# # # # # # # # # }
# # # # # # # # # adj[id,]      <- first
# # # # # # # # #
# # # # # # # # # ar            <- which(adj <= order & adj > 0, arr.ind = T) # Det her må være dårligt
# # # # # # # # #
# # # # # # # # # first.degree  <- ar[,2][ar[,1] == id]
# # # # # # # # # second.degree <- ar[,1][ar[,1] != id]
# # # # # # # # # length(unique(c(first.degree, second.degree)))
# # # # # # # # # }
# # # # # # # # #
# # # # # # # # #
# # # # # # # # #
# # # # # # # # # reach.from.adj(adj, id = 2, order = 2)
# # # # # # # # #
# # # # # # # # # # Test med stor data ----
# # # # # # # # # hoods            <- neighborhood(graph, order = 2)
# # # # # # # # # id <- 1
# # # # # # # # # hood <- hoods[[id]]
# # # # # # # # #
# # # # # # # # # graph.adj        <- as_adj(graph, attr = "weight")
# # # # # # # # #
# # # # # # # # # reach       <- function(graph, id, order){
# # # # # # # # # hood             <- ego(graph, nodes = id, order = 2)[[1]]
# # # # # # # # # adj              <- graph[as.numeric(hood), as.numeric(hood)]
# # # # # # # # # reach.from.adj(adj, id = 1, order = order)
# # # # # # # # # }
# # # # # # # # #
# # # # # # # # # graph <- graph.all
# # # # # # # # # graph <- delete.edges(graph, edges = which(E(graph)$weight >= order))
# # # # # # # # # har.reach <- which(neighborhood.size(graph, order = 2) > 1)
# # # # # # # # # system.time(sapply(har.reach[1:1000], reach, graph = graph, order = order))
# # # # # # # # #
# # # # # # # # # str(hoods)
# # # # # # # # #
# # # # # # # # #
# # # # # # # # # g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = T)
# # # # # # # # # plot(g)
# # # # # # # #
# # # # # # # # # #
# # # # # # # # # # egos <- ego(graph = g, order = order)
# # # # # # # # # # names(egos) <- V(g)$name
# # # # # # # # # # sapply(egos, function(x, g) sum(distances(g, v = names(x), to = x) <= 2.1), g)
# # # # # # # # # # sp         <- distances(g)
# # # # # # # # # # ecount(g)
# # # # # # # # # # ecount(graph)
# # # # # # # # # # elite.network()