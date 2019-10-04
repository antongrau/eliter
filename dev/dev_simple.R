# Alternative approach ----
library(eliter)
library(tidyverse)
data(pe13)


# Two mode decomposition ----

# Decompose each level completely ----
minimal.members.decomposition <- function(incidence, minimum.memberships = 3){
  
  # The incidence matrix
  inc         <- incidence
  
  # k is the minimum number of members
  k  <- 1
  
  # l.inc is a list of incidence matrices
  l.inc       <- list()
  
  # j is the minimum number of memberships for any individual
  level.up <- function(inc, k, j = 3){
    
    test.mat <- function(inc, j, k){
      
      cond                       <- !is.null(dim(inc))
      if(cond){
        cond              <- any(
          c(
            any(rowSums(inc) < j), # Is there any individuals with less than j positions
            any(colSums(inc) < k)  # Is there any affiliations with less than k members
          ))
      }
      cond
    } 
    
    while(test.mat(inc, j, k)){
      inc.t       <- inc[rowSums(inc) >= j, ]
      if(is.null(dim(inc.t))) break
      inc         <- inc.t  # Keep only those members with j or more positions
      
      inc.t       <- inc[, colSums(inc) >= k]  # Keep only those affiliations with more than k members
      if(is.null(dim(inc.t))) break
      inc         <- inc.t
    }
    inc
  }
  
  while(
    k <= min(colSums(inc)) & ncol(inc) > minimum.memberships # While k is smaller than the lowest number of members and the number of affiliations is larger than the minimum number of memberships
  ){
    k           <- k + 1
    tmp         <- level.up(inc, k, j = minimum.memberships)
    inc         <- tmp
    #if(identical(duplicate.check, TRUE)) inc <- unique.matrix(inc, MARGIN = 2)
    l.inc[[k]]  <- inc
    print(k)
    print(dim(inc))
  }
  
  # It gives us an annoying warning because level.up doesn't use a proper test of whether the inc is valid for further operation
  
  compact(l.inc)
}


level.membership <- function(l.inc){
  l.inc       <- compact(l.inc)
  membership  <- map(l.inc, rownames) %>% imap(~ tibble(Name = .x, Level = .y)) %>%
    bind_rows() %>% arrange(Name)
  
  mem         <- membership %>% group_by(Name) %>% summarise(Level = max(Level))
  mem[order(mem$Name), ]
}

level.summary <- function(l.inc){
  l.inc  <- compact(l.inc)
  l.g    <- map(l.inc, ~graph_from_incidence_matrix(incidence = .x))
  l.cl   <- map(l.g, clusters)
  
  map_dbl(l.cl, "no")  
  l.g %>% map(~bipartite.projection(.x)[[1]]) %>% map(degree) %>% map_dbl(mean)
  
}


data(den)
den         <- den[den$SOURCE != "Events",]
incidence   <- xtabs(~NAME + AFFILIATION, droplevels(den), sparse = TRUE)
l.inc       <- minimal.members.decomposition(t(incidence), 3)

level.summary(l.inc)
level.membership(l.inc)

l.inc[[5]] %>% colSums() %>% sort() %>% as.matrix()
l.inc[[5]] %>% rowSums() %>% sort() %>% as.matrix()

library(soc.ca)
library(tidyverse)

mca <- l.inc[[5]]  %>% as.matrix %>% as.data.frame()
mca <-mca == 1
mca <- soc.mca(as.data.frame(mca))
contribution(mca, 2)

#map.ind(mca)

den.corp <- den[den$SOURCE == "Corporations",]

incidence   <- xtabs(~NAME + AFFILIATION, droplevels(den.corp), sparse = TRUE)
l.inc <- minimal.members.decomposition(incidence, 2)

l.inc[[2]] %>% colSums() %>% sort() %>% as.matrix()
l.inc[[9]] %>% rowSums() %>% sort() %>% as.matrix()


# The reverse ----
incidence   <- xtabs(~NAME + AFFILIATION, droplevels(den), sparse = TRUE)
l.inc <- minimal.members.decomposition(t(incidence), 3)

l.inc[[5]] %>% colSums() %>% sort() %>% as.matrix()
l.inc[[5]] %>% rowSums() %>% sort() %>% as.matrix()

# Lets try it on career data.
load("~/Dropbox/GNA/R/elite dossier/data/Sekvens data and results.Rda") # NB! Temporary

incidence <- xtabs(~NAME + Organisation, sekvens.data, sparse = TRUE)
incidence@x <- rep(1, length(incidence@x))
l.inc <- minimal.members.decomposition(incidence, 2)

l.inc[[4]] %>% colSums() %>% sort() %>% as.matrix()
l.inc[[7]] %>% rowSums() %>% sort() %>% as.matrix()

# What is the difference to a simple degree?
library(eliter)
adj <- t(incidence) %*% incidence
tail50 <- graph_from_adjacency_matrix(adj, mode = "undirected", diag = FALSE) %>% degree() %>% sort() %>% tail(50)
setdiff(names(tail50), colnames(l.inc[[3]]))
setdiff(colnames(l.inc[[5]]), names(tail50))
# Forskellen er altså lidt mere end hver femte som er forskellig. 

# Lets try it on a bigger dataset
load("~/Dropbox/GNA/R/Ancestry/Data/den.clean.Rda")

den.set <- den.clean[den.clean$PERSON_START < "2013-12-31" & den.clean$PERSON_END >= "2014-01-01",]

incidence   <- xtabs(~NAME + AFFILIATION, droplevels(den.set), sparse = TRUE)
incidence@x <- rep(1, length(incidence@x))
l.inc <- minimal.members.decomposition(t(incidence), 3)

l.inc[[77]] %>% colSums() %>% sort() %>% as.matrix()
l.inc[[77]] %>% rowSums() %>% sort() %>% as.matrix()

# Doesn't work without merging affiliations