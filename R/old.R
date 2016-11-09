# OLD un categorized functions ----

#' Network by variable
#' 
#' Splits a network by a variable and returns matrix of descriptive values
#' @param graph is a \link{igraph} network
#' @param variabel is a factor of the same length and order as the vertices in graph
#' @return a matrix with descriptives
#' @export

network.by.variable <- function(graph, variabel){
  variabel <- as.factor(variabel)
  dele <- levels(variabel)
  output <- matrix(nrow=20, ncol=length(dele)) # Output matrix
  for ( i in 1:length(dele)){
    del <- dele[i]
    del.ind <- which(variabel==del)
    del.not <- which(variabel!=del)
    graph.del             <- graph - del.not
    
    # Antal Vertices
    Number.of.vertices  <- length(del.ind)
    # Antal edges
    Number.of.edges     <- sum(degree(graph)[del.ind])
    # Average degree
    Average.degree      <- round(Number.of.edges/Number.of.vertices, 1)
    # Part density i 1000
    Part.density        <- round(Number.of.edges/((Number.of.vertices*(vcount(graph)-1)/2))*1000, 1)
    # Clusters in part
    Number.of.clusters.in.del  <- clusters(graph.del)$no
    
    # Average path length total network
    sp                  <- shortest.paths(graph)
    ind.av.sp           <- rowSums(sp)[del.ind]/ncol(sp)
    Average.path.length <- round(sum(ind.av.sp)/length(del.ind),1)
    # Average path length within group
    sp.del                  <- shortest.paths(graph)[del.ind,del.ind]
    ind.av.sp.del           <- rowSums(sp.del)/length(del.ind)
    Average.path.length.del <- round(sum(ind.av.sp.del)/length(del.ind),1)
    
    # Longest path within group
    Longest.path.del    <- max(sp.del)
    
    # Largest number of degrees
    Largest.degree <- max(degree(graph)[del.ind])
    # Largest degree in part
    Largest.degree.del <-max(degree(graph.del))
    # Largest 2 neighborhoods
    Largest.2.neighborhood <- max(neighborhood.size(graph, 2)[del.ind])
    # Largest 3 neighborhoods
    Largest.3.neighborhood <- max(neighborhood.size(graph, 3)[del.ind])
    
    # Average closeness whole network * 10000
    Average.closeness.network    <- round(sum(closeness(graph)[del.ind])/length(del.ind) * 10000, 1)
    # Average closeness part
    Average.closeness.part       <- round(sum(closeness(graph.del))/length(del.ind) * 10000, 1)
    # Average betweenness whole network
    Average.betweenness.network  <- round(sum(betweenness(graph)[del.ind])/length(del.ind))
    # Average betweeness part
    Average.betweenness.part     <- round(sum(betweenness(graph.del))/length(del.ind))
    # Maximum betweeness whole network
    Maximum.betweenness          <- max(betweenness(graph)[del.ind])
    # Maximum closeness whole network * 10000
    Maximum.closeness            <- round(max(closeness(graph)[del.ind]) * 10000, 1)
    # Average eigenvector centrality * 1000
    Average.eigen.network        <- round(sum(evcent(graph)$vector[del.ind])/length(del.ind) * 1000, 1)
    # Maximum eigenvector centrality
    Maximum.eigen                <- round(max(evcent(graph)$vector[del.ind])* 1000, 1)
    
    del.stat <- c(Number.of.vertices, Number.of.edges, Average.degree, Part.density, Number.of.clusters.in.del,
                  Average.path.length, Average.path.length.del, Longest.path.del, Largest.degree, Largest.degree.del,
                  Largest.2.neighborhood, Largest.3.neighborhood,
                  Average.closeness.network, Average.closeness.part, Maximum.closeness,
                  Average.betweenness.network, Average.betweenness.part, Maximum.betweenness,
                  Average.eigen.network, Maximum.eigen)
    
    
    output[,i] <- round(del.stat, 1)
  }
  colnames(output) <- dele
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (o/oo)", "Number of clusters in part",
                        "Average path length", "Average path length in part", "Longest path in part", "Highest degree", "Highest degree in part",
                        "Largest 2. neighborhood", "Largest 3. neighborhood",
                        "Average closeness", "Average closeness in part", "Maximum closeness",
                        "Average betweeness", "Average betweenness in part", "Maximum betweenness",
                        "Average eigencentrality", "Maximum eigencentrality")
  return(output)
}
############# Endnu en beskrivende funktion
describe.network <- function(graph, variabel, org.data){
  
  #ALLE
  between       <- betweenness(graph)
  neighborhood.size.3 <- neighborhood.size(graph, 3)
  degrees       <- degree(graph)
  core.com      <- clusters(graph) 
  core.com.mem  <-  core.com$membership==which.max(core.com$csize)
  
  nvertex.all                 <- vcount(graph)
  nedges.all                  <- ecount(graph)
  percentage.in.largest.com   <- sum(core.com.mem)/nvertex.all * 100
  Average.degree              <- sum(degrees)/nvertex.all
  Average.betweenness         <- sum(between)/nvertex.all
  Average.3.neighborhood.size <- sum(neighborhood.size.3)/nvertex.all
  
  result.matrix <- as.data.frame(matrix(nrow = 6, ncol=1+nlevels(variabel)))
  res.all       <- c(nvertex.all, nedges.all, percentage.in.largest.com, Average.degree, Average.betweenness, Average.3.neighborhood.size)
  result.matrix[,1] <- res.all
  
  levels.variabel <- levels(variabel)
  
  # Del
  for( i in 1:nlevels(variabel)){
    graph.part               <- graph - which(variabel!=levels.variabel[i])
    part.ind                 <- which(variabel==levels.variabel[i]) 
    between.part             <- between[part.ind]
    neighborhood.size.3.part <- neighborhood.size.3[part.ind]
    degrees.part             <- degrees[part.ind]
    core.com.mem.part        <- core.com.mem[part.ind]
    
    nvertex.part                     <- vcount(graph.part)
    nedges.part                      <- ecount(graph.part)
    percentage.in.largest.com.part   <- sum(core.com.mem.part)/nvertex.part * 100
    Average.degree.part              <- sum(degrees.part)/nvertex.part
    Average.betweenness.part         <- sum(between.part)/nvertex.part
    Average.3.neighborhood.size.part <- sum(neighborhood.size.3.part)/nvertex.part
    
    res.part       <- c(nvertex.part, nedges.part, percentage.in.largest.com.part, Average.degree.part, Average.betweenness.part, Average.3.neighborhood.size.part)
    result.matrix[,i+1] <- res.part  
  }  
  
  colnames(result.matrix) <- c("All", levels.variabel)
  rownames(result.matrix) <- c("Corporations", "Ties", "% in central component", "Average degree", "Average betweeness", "Average 3rd neighborhoodsize")
  
  round(result.matrix, 1)
}

####################### Describe vertex

# Den her funktion skal laves om - den skal hente resultater ind fra nogle allerede eksisterende analyser


#' Create an organisation variable from relations matrix 
#' 
#' @param rel is a relations matrix - or a affiliation edge list
#' @param net is an igraph network object
#' @param var is a variable of the same length and order as rel - often it would be "SOURCE" or "TAG"
#' @return a character vector
#' @export

variable.from.rel.org  <- function(den, graph){
  d                    <- data.frame(AFFILIATION = I(V(graph)$name))
  den$AFFILIATION      <- as.character(den$AFFILIATION)
  den.uni              <- den[duplicated(den$AFFILIATION) == FALSE,]
  left_join(d, den.uni, by = "AFFILIATION")
}

edge.neighborhood  <- function(graph){
  el               <- get.edgelist(graph)
  edge.neighbors   <- apply(el, 1, neighbors, graph = graph)
  sapply(edge.neighbors, length)
}

edge.neighborhood.intersection <- function(graph){
  vs                     <- V(graph)
  el                     <- E(graph)
  connections            <- lapply(vs, neighbors, graph = graph)
  
  edge.overlap           <- function(el.row, connections){
    edge.connections       <- connections[ends(graph, el.row, names = FALSE)]
    length(do.call("intersection", edge.connections))
  }
  sapply(el, edge.overlap, connections = connections)
}


# en          <- edge.neighborhood.intersection(graph)
# dist.plot(en)
# graph.plot(graph, edge.color = en, edge.alpha = en) + scale_color_gradient(high = "darkblue", low = "papayawhip")
