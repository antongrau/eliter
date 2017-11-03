library(ggraph)
library(eliter)
data(pe13)

layout <- cbind(V(net.elite)$layout.x, V(net.elite)$layout.y)
g