# library(eliter)
# data(den)
# den  <- has.tags(den, "Health", result = "den")
# den <- as.den(den)
# 
# 
# increase.memberships <- function(den){
# snit     <- mean(table(den$NAME))
# n.name   <- length(table(den$NAME))
# 1.5 * n.name
# n.name
# n.name / 1.5
# 
# snit.seq        <- seq(from = snit, to = 2.5, by = 0.01)
# nak.seq         <- round(n.name - (n.name / snit.seq))
# 
# 
# nak.i.den <- function(den, n){
# 
# tab         <- table(den$NAME)
# non.linkers <- names(tab)[tab < 2]
# nak         <- sample(non.linkers, n)
# 
# nak.den     <- den[den$NAME %in% nak,]
# den         <- den[!den$NAME %in% nak,]
# ud.med.dig  <- sample(nak.den$NAME, n/2)
# nak.den$NAME[nak.den$NAME %in% ud.med.dig] <- sample(!nak.den$NAME %in% ud.med.dig, n/2)
# rbind(den, nak.den)
# }
# 
# nak.list       <- lapply(nak.seq, nak.i.den, den = den)
# graph.list     <- lapply(nak.list, elite.network.org)
# res            <- sapply(graph.list, share.of.paths)
# plot(res)
# share.of.paths <- function(x){
# sp <- shortest.paths(x, mode = "out")
# sum(sp <= 2.1) / length(sp)
# }
# 
# bet.cent  <- sapply(graph.list, function(x) centr_betw(x)$centralization)
# plot(bet.cent)
# 
# density  <- sapply(graph.list, graph.density)
# plot(density)
# 
# n.n2     <- sapply(graph.list, function(x) mean(ego_size(x, order = 2)))
# plot(n.n2)
