library(eliter)

# We want to assign edge weights to a graph on the basis of the edge.betweenness.estimate
# The graph has multiple edges or is unweighted. But for our purpose multiple edges the most important.

library(eliter)
edge.list <- c("A", "B")

g <- graph_from_literal( A-B,
                    A-B,
                    A-C,
                    A-C,
                    B-C,
                    B-C,
                    A-X,
                    A-X,
                    A-X,
                    A-X,
                    A-X,
                    X-Y,
                    X-Z,
                    Z-Y,
                    W-Z,
                    Y-W,
                    X-W,
                    
                    simplify = FALSE)

plot(g)

# We want A - B - C and Z - X - Y to have the same weights. X - A has to be larger.
# We might accept that A - B and A - C is higher. They are the bridgers to a redundent set.
# But right now A-B and A-C are higher than Y - X and Z - X. 
# We should be able to ignore that they are connected to 

eb <- edge.betweenness.estimate(g, cutoff = 2, weights = NA)
E(g)$weight <- eb

data.frame(get.edgelist(g), weight = E(g)$weight)

sg <- simplify(g, edge.attr.comb = "sum")
be <- betweenness.estimate(sg, weights = NA, cutoff = 2)

graph.plot(sg, edge.text = E(sg)$weight, text = T, text.size = 5, vertex.fill = "white", vertex.size = be)

E(simplify(g))$weight

# Størrere eksempel ----
g <- graph_from_literal( A-B,
                         A-B,
                         A-C,
                         A-C,
                         B-C,
                         B-C,
                         A-X,
                         A-X,
                         A-X,
                         A-X,
                         A-X,
                         X-Y,
                         X-Z,
                         Z-Y,
                         W-Z,
                         Y-W,
                         X-W,
                         
                         X-J,
                         J-K,
                         M-J,
                         K-L,
                         L-M,
                         M-X,
                         J-L,
                         K-M,
                         X-L,
                         X-K,
                         
                         
                         simplify = FALSE)


plot(g)

# We want A - B - C and Z - X - Y to have the same weights. X - A has to be larger.
# We might accept that A - B and A - C is higher. They are the bridgers to a redundent set.
# But right now A-B and A-C are higher than Y - X and Z - X. 
# We should be able to ignore that they are connected to 

eb <- edge.betweenness.estimate(g, cutoff = 2, weights = NA)
E(g)$weight <- eb

data.frame(get.edgelist(g), weight = E(g)$weight)

sg <- simplify(g, edge.attr.comb = "sum")
be <- betweenness.estimate(sg, weights = NA, cutoff = 2)

graph.plot(sg, edge.text = E(sg)$weight, text = T, text.size = 5, vertex.fill = "white", vertex.size = be)

E(simplify(g))$weight

?adjacent_vertices
incident_edges(sg, )
