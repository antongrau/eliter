library(ggraph)
library(tidygraph)
library(eliter)
data(pe13)

layout <- cbind(V(net.elite)$layout.x, V(net.elite)$layout.y)

g <- as_tbl_graph(net.elite)

p <- ggraph(g) + geom_conn_bundle() + 
  geom_node_point() + 
  ggtitle('An example')
p

library(igraph)
flareGraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
importFrom <- match(flare$imports$from, flare$vertices$name)
importTo <- match(flare$imports$to, flare$vertices$name)
flareGraph <- tree_apply(flareGraph, function(node, parent, depth, tree) {
  tree <- set_vertex_attr(tree, 'depth', node, depth)
  if (depth == 1) {
    tree <- set_vertex_attr(tree, 'class', node, V(tree)$shortName[node])
  } else if (depth > 1) {
    tree <- set_vertex_attr(tree, 'class', node, V(tree)$class[parent])
  }
  tree
})
V(flareGraph)$leaf <- degree(flareGraph, mode = 'out') == 0

ggraph(flareGraph, layout = "nicely") +
  geom_conn_bundle(aes(colour = ..index..), data = get_con(importFrom, importTo),
                   edge_alpha = 0.25) +
  geom_node_point(aes(filter = leaf, colour = class)) +
  scale_edge_colour_distiller('', direction = 1, guide = 'edge_direction') +
  coord_fixed() +
  ggforce::theme_no_axes()

graph.plot(flareGraph)

