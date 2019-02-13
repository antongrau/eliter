library(eliter)

data(den)
graph.all  <- elite.network(den)
graph.linkers <- largest.component(graph.all)
graph.linkers <- betweenness.decomposition(graph.linkers)

# Reach dependency
# Hvor afhængig er ens rækkevidde af ens stærkeste forbindelse?
# Hvad gør vi ved at der allerede er en vægtet forbindelse? Skal vi gange afhængigheden med styrken på forbindelsen?

# Lad os starte med en simpel udgave hvor vi slagter svage forbindelser og sætter det hele til 0 og 1
g            <- graph.linkers
data(pe13)
g            <- net.elite
g            <- delete.edges(g, which(E(g)$weight > 1))
E(g)$weight  <- 1


reach        <- reach(g)
deg          <- graph.strength(g, weights = 1/E(g)$weight)

a            <- g[]
a.deg        <- a * deg

a.max        <- rowMax(a.deg)

max(a.max)
max(deg)
max(reach)


dependency <- (a.max/reach)

d <- data.frame(name = V(g)$name, reach = reach, degree = deg, a.max = as.vector(a.max), dependency = dependency)
View(d)