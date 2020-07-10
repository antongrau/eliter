# Functions for sequences -----

#' Title
#'
#' @param data the sequence data set
#'
#' @return
#' @details data needs to have the following format: start and end in years. organization is the name of the organization.
#' @export
#'
#' @examples
#' data("careers")
#' dt        <- sequence.data_raw[!gsub(",", ".", sequence.data_raw$`Career level`) %in% c(0.3, 0.5, 0.6, 0.7, 0.8,0.9),]
#' dt        <- data.frame(name = dt$NAME, organization = dt$Organisation, start = dt$Start, end = dt$End)
#' plot_cum_ind_pr_org(dt)

plot_cum_ind_pr_org <- function(data, reference.year = 2013, cut.points = c(0.5, 0.75, 0.9, 0.95), x.position = 200) {
  
  data      <- na.omit(data)
  data      <- data %>% mutate(duration = (end - start) + 1, present.in.reference = end == reference.year)
  org.stat  <- data %>% group_by(organization, name) %>% summarise(duration.in.position = sum(duration), present = sum(present.in.reference))
  org.stat  <- org.stat %>% summarise(years.in.org = sum(duration.in.position), pers.in.org = n(), present = sum(present))
  org.stat  <- org.stat %>% arrange(-pers.in.org) %>% mutate(cum.sum.pers = cumsum(pers.in.org), cum.sum.pers.share = cumsum(pers.in.org)/sum(pers.in.org))

  # Persons
  persons <- tibble(x = 1:length(unique(org.stat$organization)), nb = NA, nb_still = NA, cum_pct_ind = NA)
  
  i <- 1
  for(i in 1:nrow(persons)){
    persons$nb[i]                 <- length(unique(data$name[data$organization %in% org.stat$organization[1:i]]))
    persons$nb_still[i]           <- length(unique(data$name[data$end == reference.year & data$organization %in% org_stat$organization[1:i]]))
    persons$cum_pct_ind[i]        <- length(unique(data$name[data$organization %in% org.stat$organization[1:i]])) / length(unique(data$name))
  }
  
  # creating 'cut points' for the graph
  
  f <- function(persons, x){
    nrow(persons) - sum(persons$cum_pct_ind >= x) + 1
  }
  
  cp      <- map(cut.points, ~f(persons, .x)) %>% unlist()
  seg.dat <- tibble(cp = cp, share = cut.points)
  
  p <- ggplot() + geom_line(data = persons, aes(x = 1:nrow(persons), y = cum_pct_ind), size = 0.6, alpha = 0.8,) 
  p <- p + scale_y_continuous(expand = c(0,0), breaks = c(0, 25, 50, 75, 90, 95,100)/100, labels = percent, minor_breaks = seq(0, 100, 12.5))
  p <- p + geom_segment(data = seg.dat, mapping = aes(x = 0, y = share, xend = cp, yend = share), size = 0.4, alpha = 0.8)
  p <- p + geom_segment(data = seg.dat, mapping = aes(x = cp, y = 0, xend = cp, yend = share), size = 0.4, alpha = 0.8)
  
  p <- p + xlab('Nb of organizations (orderd by Nb of individuals)')
  p <- p + ylab(paste0('Cumulated % unique individuals', " (N = ", length(unique(data$name)), ")", sep = ""))
  p <- p + theme_classic() + theme(text = element_text(family = "serif"))

  breaks  <- c(1, 100, 200, 300, 400, 500, 600, length(unique(org.stat$organization)),
               cp) %>% sort()
  
  
  p1 <- p1 + scale_x_continuous(expand = c(0,0), breaks = breaks, 
                                labels = breaks,
                                minor_breaks = seq(0, nrow(org.stat), 100))
  
  p1 <- p1 + labs(title = "", family  = "serif")
  
  # Plotting table
  t.m <- org.stat %>% select(Organization = organization, Years = years.in.org, Persons = pers.in.org, `Still active` = present)
  t.p <- t.m %>% mutate(Organization = as.character(Organization))
  t.p <- t.p[1:cp[1], ]  
  t.p[nrow(t.p) + 1,] <- "..."
  t.p[nrow(t.p) + 1,] <- c("Total", t.m[1:cp[1], ] %>% select(-Organization) %>% colSums())
  t.p[nrow(t.p) + 1,] <- "..."
  t.p[nrow(t.p) + 1,] <- c("Grand total", sum(org.stat$years.in.org), n_distinct(data$name), n_distinct(data$name))
  tab <- t.p
  
  
  just                  <- matrix(NA, nrow = nrow(tab), ncol = ncol(tab))
  just.x                <- just
  just[,1]              <- 0
  just.x[,1]            <- 0.05
  just[,2:ncol(just)]   <- 1
  just.x[,2:ncol(just)] <- 0.9
  
  tbody.style <- tbody_style(color = "black", face = "plain", size = 8, hjust = just, x = just.x, fill = "white") 
  colnames.style <- colnames_style(color = "black", face = "bold", fill =  "white",size = 10, hjust = c(0,1,1,1), x = c(0.05,0.9,0.9,0.9))
  stable.p <- ggtexttable(tab, rows = NULL, theme = ttheme(tbody.style = tbody.style, padding = unit(c(3,3), "mm"), colnames.style = colnames.style), cols = c("Organisation","Years", "Persons", paste0("Still active (", reference.year, ")")))
  stable.p  <- stable.p + theme(text = element_text(family = "serif"))
  
  p1 <- p + annotation_custom(ggplotGrob(stable.p),
                              ymax = 0.95,
                              xmin = x.position)
  p1
}





#' Career stairs with fill
#'
#' @param stair.seq 
#' @param fill.seq 
#' @param org.seq 
#' @param name 
#' @param reference 
#' @param hline 
#' @param colored 
#' @param base_size 
#' @param fill.colors 
#'
#' @return
#' @export
#'
#' @examples
# data("careers")
# ff      <- seqrep.grp(seqdata = seq.list$seq.sec, criterion = "dist", nrep = 3 , diss = dist.mat, group = clusters, with.missing = T, ret = "rep")
# medoids <- lapply(ff, function(x) dimnames(attr(x, "Distances"))[[2]])
# 
# 
# 
# 
# stair.seq <- seq.list$seq.level
# fill.seq  <- seq.list$seq.sec
# org.seq   <- seq.list$seq.neworg
# size.seq  <- seq.list$seq.size
# geo.seq   <- seq.list$seq.region
# facet     <- NULL
# 
# 
# 
# 
# filled.stairs <- function(stair.seq, fill.seq, org.seq, size.seq, geo.seq, reference = length(alphabet(stair.seq)), facet = NULL, fill.colors = brewer.pal(n = 9, name = "Set1")){
#   
#   id.vars      <- "name"
#   
#   to.melt      <- function(x, id.vars, facet = NULL){
#     xs           <- data.frame(name = rownames(x), x)
#     
#     if (is.null(facet) == FALSE){
#       xs$facet <- facet
#       id.vars     <- c(id.vars, "facet")
#     }
#     
#     ms           <- melt(xs, id.vars = id.vars)
#     ms$time      <- ms$variable
#     
#     ms$org.value <- ms$value
#     ms$value     <- factor(ms$value, levels = alphabet(x), ordered = TRUE)
#     ms$level     <- as.numeric(ms$value)
#     ms
#   }
#   
#   ms           <- to.melt(stair.seq, id.vars, facet = facet)
#   ms.fill      <- to.melt(fill.seq, id.vars, facet = facet) 
#   ms.org       <- to.melt(org.seq, id.vars, facet = facet)
#   ms.size      <- to.melt(size.seq, id.vars, facet = facet)
#   ms.geo       <- to.melt(geo.seq, id.vars, facet = facet)
#   
#   ms$fill      <- ms.fill$value
#   ms$org       <- ms.org$value
#   ms$size      <- ms.size$value
#   ms$geo       <- ms.geo$value
#   
#   ms$name      <- factor(ms$name, levels = rownames(stair.seq), ordered = TRUE)
#   
#   ms$time      <- ms$variable
#   ms           <- ms[ms$org.value != "*",]
#   
#   levels(ms$time) <- gsub(pattern = "y|a", replacement = "", x = levels(ms$time))
#   ms$time      <- as.numeric(as.character(ms$time))
#   
#   # ms           <- ms %>% group_by(name) %>% mutate(first.reference = level %in% reference  & !duplicated(level %in% reference & is.na(level) == FALSE))
#   # ms$reference <- ms$level
#   # ms$reference[ms$first.reference == FALSE] <- NA 
#   
#   
#   ms$colored   <- ms$value
#   levels(ms$colored)[!levels(ms$colored) %in% colored] <- "Level"
#   
#   color.names  <- c("Level", colored)
#   color.scale  <- c("black", grey.colors(n = length(colored), start = 0.75, end = 0.4))
#   names(color.scale) <- color.names
#   
#   ms.org   <- ms[which(ms$org == "New org"),]
#   ms.org$size <- factor(ms.org$size %in% c("V", "L"), levels = c("TRUE", "FALSE"), labels = c("Large", "Medimum/Small"))
#   
#   sc       <- list()
#   sc$y     <- scale_y_discrete(expand = c(0.1, 0.1))
#   sc$fill  <- scale_fill_manual(values = fill.colors, name = "Sector", drop = FALSE)
#   sc$size  <- scale_size_manual(values = c(2.5, 1), name = "Organisation size", drop = FALSE)
#   sc$shape <- scale_shape_manual(values = c(23, 21, 25, 24), name = "Geography", drop = FALSE)
#   
#   p    <- ggplot(data = ms, aes(x = as.numeric(time), y = level, group = name, fill = fill)) 
#   p    <- p + geom_bar(stat = "identity", color = NA, width = 1)
#   p    <- p + geom_step(size = 0.3, aes(x = time-0.5))
#   p    <- p + geom_segment(data = ms.org, aes(x = time-0.5, xend = time-0.5, yend = level, y = 0), color = "black", size = 0.5)
#   p    <- p + geom_point(data = ms.org, aes(x = time-0.5, y = level, shape = geo, size = size), color = "black", fill = "black")
#   
#   p    <- p + xlab("") + ylab("")
#   p    <- p + theme_fem(axisColor = "black", size = 6) + theme(axis.ticks = element_line(color = "black"), axis.text = element_text(angle = -45, hjust = 0.5))
#   
#   if(is.null(facet)) p    <- p + facet_wrap(~name, ncol = 1)
#   if(is.null(facet) == FALSE) p <- p + facet_wrap(vars(name, facet), scales = c("free_x")) + sc
#   
#   p    <- p + sc
#   p 
# }
# 
# skalaer <- c("Business"= "YlGnBu",  "Business association" = "YlGnBu", "Politics" = "YlGn", "Science/Education" = "YlOrBr", 
#   "State administration" = "PuRd", "Union" = "YlOrRd")
# skilling.skalaer              <- lapply(skalaer, brewer.pal, n = 9)
# niveau                        <- 4
# skilling.colors <-   c("Business association" = skilling.skalaer$`Business association`[niveau - 0],
#                        "Business" = skilling.skalaer$`Business`[niveau + 2],
#                        "Science/Education" = skilling.skalaer$"Science/Education"[niveau -1],
#                        "Politics" = skilling.skalaer$Politics[niveau + 2],
#                        "State administration" = skilling.skalaer$`State administration`[niveau + 1],
#                        "Union" = skilling.skalaer$Union[niveau + 2])
# 
# 
# out <- list()
# for (i in 1:length(medoids)){
# 
# sm       <- stack(medoids[i])
# 
# p        <- filled.stairs(stair.seq[sm$values, ], fill.seq[sm$values, ], org.seq[sm$values, ], size.seq[sm$values, ], geo.seq[sm$values, ])
# p        <- p + ggtitle(names(medoids[i])) + scale_fill_manual(values = skilling.colors, drop = FALSE)
# p        <- p + scale_x_continuous(limits = c(NA, 70)) + scale_y_continuous(limits = c(0,8), )
# out[[i]] <- p + theme(legend.position = "none", axis.text = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), title = element_text(size = 6)) 
# 
# 
# }
# 
# out[[6]] <- out[[6]] + theme(legend.position = "right")
# 
# out[[6]]
# out[[10]]
# 
# library(patchwork)
# names(out) <- names(medoids)
# nif <- wrap_plots(out, ncol = 2)
# nif
