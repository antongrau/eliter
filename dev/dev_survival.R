# Spell graph example -----
load("~/Desktop/spell.Rda")
library(eliter)
samp         <- sample(1:vcount(spell.graph), size = vcount(spell.graph)*0.9)
spell.graph  <- delete.vertices(spell.graph, v =  samp)


prior        <- prior.connections(spell.graph)
prior.rle    <- lapply(prior, rle)

prior.duration <- sapply(prior.rle, function(x) x$lengths[1])
gap.duration   <- sapply(prior.rle, function(x) x$lengths[2])
number.of.gaps <- sapply(prior.rle, function(x) length(x$lengths))

x <- prior.rle$`Poul Thorup %--% Verner Bach Pedersen`

rle.to.data.frame <- function(x){
  l      <- length(x$lengths)
  d      <- data.frame(lengths = x$lengths[-l],
                       values  = x$values[-l],
                       n.break = rep(seq(1:(l-1)/2), each = 2)
  ) 
  d
  
}


prior.rle.dat <- lapply(prior.rle, rle.to.data.frame)



cor(prior.duration, gap.duration)

d              <- data.frame(prior.duration, gap.duration)

library(ggthemes)

p              <- ggplot(d, aes(y = gap.duration, x = prior.duration)) + geom_point(size = 0.1) + geom_smooth(color = "red")
p              <- p + geom_rangeframe() + theme_tufte() 
p

# Survival ! ------


# Datasættet skal have:
# For alle afsluttede ties!
# Dvs. en spell.graph hvor alle ties er afsluttede i 2010 - så har de haft 6 år til at genopstå
# Men vi skal splitte så ties der reemerger flere gange der skal vi have dem delt op!
# Vi skal bruge:
# Tie reemergence! TRUE/FALSE for dem der har en prior!
# Tie duration
# Break duration - hvis de aldrig genopstår skal den være fra break til 01-01-2016


from.spell.graph.to.survival <- function(spell.graph, end.month = 1392){

# reference.month <- spell.graph$reference.month
# reference.month + months(1392)
# reference.month + months(1100)

gs          <- delete.edges(spell.graph, edges = which(E(spell.graph)$end >= end.month))
gs          <- simplify(gs, remove.multiple = FALSE, remove.loops = TRUE)

# Data for the ties without reemergence -----
gs.no.reemergence   <- delete.edges(gs, which(count_multiple(gs) > 1))
 
data.no.reemergence <- data.frame(remergence     = FALSE,
                                  duration       = E(gs.no.reemergence)$end - E(gs.no.reemergence)$start,
                                  break.duration = end.month - E(gs.no.reemergence)$end,
                                  start.month    = E(gs.no.reemergence)$start
                                  )

gender.a <- code.gender(firstnames(head_of(gs.no.reemergence, E(gs.no.reemergence))$name))
gender.b <- code.gender(firstnames(tail_of(gs.no.reemergence, E(gs.no.reemergence))$name))

data.no.reemergence$gender.similarity <- gender.a == gender.b

# Data for the ties that reemerge ----
prior        <- prior.connections(gs, minimum.gap = 0)
prior.rle    <- lapply(prior, function(x) rle(as.vector(x)))
prior.start  <- sapply(prior, function(x) attributes(x)$start)
prior.end    <- sapply(prior, function(x) attributes(x)$end)

prior.dat   <- list()

pb           <- txtProgressBar(min = 1, max = length(prior.rle), style = 3)
for (i in 1:length(prior.rle)) {
  
  # Add the length of the final break
  l                <- length(prior.rle[[i]]$lengths)
  x                <- prior.rle[[i]]
  x$lengths[l + 1] <- end.month - prior.end[i]
  x$values[l + 1]  <- FALSE 

  # Gender
  n                <- names(prior.rle)[i]
  n                <- unlist(str_split(n, pattern = " %--% ", n = 2))
  n                <- firstnames(n)
  n                <- as.character(code.gender(n))
  n[is.na(n)]      <- "FALSE"
  n                <- n[1] == n[2]
  
  # Collect
  out       <- data.frame(remergence     = TRUE,
                          duration       = x$lengths[x$values],
                          break.duration = x$lengths[x$values == FALSE],
                          start.month    = as.numeric(prior.start[i]),
                          gender.similarity = n)  
  # The final break does not remerge
  out$remergence[nrow(out)]   <- FALSE
  
  prior.dat[[i]] <- out
  setTxtProgressBar(pb = pb, value = i)
} 

data.reemergence <- bind_rows(prior.dat)

data.all         <- rbind(data.reemergence, data.no.reemergence)

data.all$remergence <- as.numeric(data.all$remergence)
data.all$gender.similarity <- as.numeric(data.all$gender.similarity)

data.all
}


write.csv(data.all, file = "~/Dropbox/GNA/R/survival/data/survival.csv")

nif <- read.csv(file = "~/Dropbox/GNA/R/survival/data/survival.csv")



summary(nif$duration)

quantile(nif$duration, probs = (1:100)/100)

# LM ----
colnames(data.all)
model            <- lm(remergence ~ break.duration + duration, data = data.all)
summary(model)


# Survival analysis ----
library(OIsurv)

?Surv

