library(eliter)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggpubr)


# Data ind ----
data(den)   # Den her kan også ryge
den         <- read_delim(file = "", sep = ";")
den         <- has.tags(den, "Media", result = "den") # Den her linje ud!

# Til netværk ----
graph.ind   <- elite.network(den)
graph.affil <- elite.network(den, result = "affil")
graph.two   <- elite.network(den, result = "two.mode")

graph.com   <- largest.component(graph.ind, cut.off = 1)

min(V(graph.com)$weighted.memberships) # Alle har mere end 1 vægtet medlemskab

graph.affil.com   <- largest.component(graph.affil)
graph.two.com     <- largest.component(graph.two)

# Centralitetsmål ----
vm.ind      <- vertex.measures(graph.com)
vm.ind$Medlemskaber <- membership.vector(V(graph.com)$name, den)
vm.ind      <- cbind(Navn = V(graph.com)$name, vm.ind)

vm.org    <- vertex.measures.directed(graph.affil.com)
vm.org$Medlemmer   <- members.vector(V(graph.affil.com)$name, den)
vm.org     <- cbind(Navn = V(graph.affil.com)$name, vm.org)

# Netværk ----
reach.ind        <- reach(graph.ind)
p.ind            <- graph.plot(graph.ind, vertex.fill = reach.ind, edge.color = "black", edge.size = 0.1, vertex.size = reach.ind, edge.alpha = 0.5)
p.ind            <- p.ind + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.ind            <- p.ind + scale_size(range = c(1, 4), guide = "none")

reach.affil        <- reach(graph.affil)
p.affil            <- graph.plot(graph.affil, vertex.fill = reach.affil, edge.color = "black", edge.size = 0.1, vertex.size = reach.affil, edge.alpha = 0.5)
p.affil            <- p.affil + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.affil            <- p.affil + scale_size(range = c(1, 4), guide = "none")

reach.two        <- reach(graph.two)
p.two            <- graph.plot(graph.two, vertex.fill = reach.two, edge.color = "black", edge.size = 0.1, vertex.size = reach.two, edge.alpha = 0.5, vertex.shape = V(graph.two)$type)
p.two            <- p.two + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.two            <- p.two + scale_size(range = c(1, 4), guide = "none") + scale_shape_manual(values = c(21, 22), name = "Fora", labels = c("Person", "Fora"))


# Linkers ----
reach.com        <- reach(graph.com)
p.com            <- eliter:::graph.plot.repel(graph.com, vertex.fill = reach.com, edge.color = "black", edge.size = 0.1, vertex.size = reach.com, edge.alpha = 0.5,
                               text = T)
p.com            <- p.com + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.com.repel      <- p.com + scale_size(range = c(1, 4), guide = "none")
p.com.repel

p.com            <- graph.plot(graph.com, vertex.fill = reach.com, edge.color = "black", edge.size = 0.1, vertex.size = reach.com, edge.alpha = 0.5,
                                              text = T, text.background = "white")
p.com            <- p.com + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.com            <- p.com + scale_size(range = c(1, 4), guide = "none")
p.com

reach.com.affil        <- reach(graph.affil.com)
p.com.affil            <- eliter:::graph.plot.repel(graph.affil.com, vertex.fill = reach.com.affil, edge.color = "black", edge.size = 0.1, vertex.size = reach.com.affil, edge.alpha = 0.5,
                               text = T)
p.com.affil            <- p.com.affil + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.com.affil.repel      <- p.com.affil + scale_size(range = c(1, 4), guide = "none")
p.com.affil.repel

reach.com.affil        <- reach(graph.affil.com)
p.com.affil            <- graph.plot(graph.affil.com, vertex.fill = reach.com.affil, edge.color = "black", edge.size = 0.1, vertex.size = reach.com.affil, edge.alpha = 0.5,
                                     text = T, text.background = "white")
p.com.affil            <- p.com.affil + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.com.affil            <- p.com.affil + scale_size(range = c(1, 4), guide = "none")
p.com.affil

# Two mode linkers -----
graph.two.link          <- betweenness.decomposition(graph.two.com)

reach.two.link          <- reach(graph.two.link)
p.link.two              <- graph.plot(graph.two.link, vertex.fill = reach.two.link, edge.color = "black", edge.size = 0.1, vertex.shape = V(graph.two.link)$type, vertex.size = reach.two.link, edge.alpha = 0.5,
                                     text = T, text.background = "white")
p.link.two             <- p.link.two + scale_fill_viridis_c(option = "magma", direction = -1, name = "Rækkevidde")
p.link.two             <- p.link.two + scale_size(range = c(1, 4), guide = "none") + scale_shape_manual(values = c(21, 22), name = "Fora", labels = c("Person", "Fora"))
p.link.two

# Ego netværk ----
pl.ego                 <- lapply(V(graph.com)$name, ego.two.mode, den = den, member.of = V(graph.two.link)$name)
pl.ego.small           <- lapply(V(graph.com)$name, ego.two.mode, den = den, member.of = V(graph.two.link)$name, text = FALSE)
pl.ego.small           <- lapply(pl.ego.small, "+", scale_size(range = c(1, 3), guide = "none"))

# PDF -----
pl <- list(p.two + ggtitle("Fora og individer sammen"),
     p.ind + ggtitle("Individer"),
     p.affil + ggtitle("Fora"),
     p.com + ggtitle("Linkers: Individer der sidder mere end 1 sted", subtitle = "I den største komponent"),
     p.com.repel + ggtitle("Linkers: Individer der sidder mere end 1 sted", subtitle = "I den største komponent"),
     p.com.affil + ggtitle("Linkers: Fora der forbinder", subtitle = "I den største komponent"),
     p.com.affil.repel + ggtitle("Linkers: Fora der forbinder", subtitle = "I den største komponent"),
     p.link.two + ggtitle("De afgørende fora og individer", subtitle = "Kun unikke brobyggere")
     )

pdf(file = "Netværk.pdf", height = 10, width = 10)
pl
dev.off()

cairo_pdf(file = "Ego.pdf", height = 10, width = 10, onefile = TRUE)
pl.ego
dev.off()

cairo_pdf(file = "Ego_små.pdf", height = 20, width = 20, onefile = TRUE)
ggarrange(plotlist = pl.ego.small)
dev.off()


# CSV -----
write_delim(vm.ind, path = "Individer.csv", delim = ";")
write_delim(vm.org, path = "Fora.csv", delim = ";")
