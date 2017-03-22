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
                    
                    simplify = FALSE)

plot(g)

eb <- edge.betweenness.estimate(g, cutoff = 5)


E(g)$weight <- eb

graph.plot(simplify(g))

E(simplify(g))$weight
