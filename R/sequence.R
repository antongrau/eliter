# Functions for sequences -----




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
