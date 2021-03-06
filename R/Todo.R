# Lav et script der gemmer navne datasættet
# Dokumenter de sidste funktioner
# Lav et eksempel på udtræk for individer, organisationer og for tags
# Gem resultater så de kan bruges i andre analyser
# Flere datasæt: Inner Circle - Firmaets Mænd - Top embedsmænd

# Lister af individer eller affiliations - de lister skal kunne bruges til at konstruere sektor degree
# Vi kan forestille os: 
# Adelige
# Politikere
# Forskere
# Professorer
# Fagforeningsledere
# Folk med tillidsposter i fagbevægelsen
# Top 1000 direktører
# Top 1000 direktører og bestyrelsesformænd
# DI - Tillidsposter og udvalgsposter
# Ordner - Riddere af Dannebrog

# I graph.plot skal vi skifte fra geom_text til geom_label til at lave vertex labels. 
# Lige nu har vi problemer med at aligne labels og background

# Vi vil have nye layout funktioner: 
# Specifikt vil vi kunne lave louvain clusters.

# Vi vil have convex.hulls og koncentrationsellipser til at annotere vores plots

# Vi kan lave farvede edges: En edge tegnes kun halvvejs, her tegnes den efter vertex.fill for ego. Den næste halvdel tegnes efter alters vertex.fill.










########################################################################
# Find the elite - eksempel:
# library(soc.elite)
# data(den)
# net.all                 <- elite.network(den, sigma = 14)
# net.com                 <- largest.component(net.all, cut.off = 1)
# sp                      <- shortest.paths(net.com)
# sp.2                    <- sp <= 2.1 * 1
# net.sp                  <- graph.adjacency(sp.2, mode="undirected", diag=FALSE)
# core.sp                 <- graph.coreness(net.sp)
# kore                    <- core.sp == max(core.sp)
# l                       <- vcount(net.com) + 1
# sec                     <- vector(length = l - 1)
# secondary               <- secondary.actors(x = core.sp, rel.all = den)
# sec[kore]               <- secondary
# sec[sec == "FALSE"]       <- ""
# sec[V(net.com)$name == "Prins Henrik"] <- ""
# elite.names             <- V(net.com)$name[kore & sec == ""]
# net.elite               <- net.com - which((kore & sec == "") == FALSE)
# 
# 
