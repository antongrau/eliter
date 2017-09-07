library(eliter)
load("~/Dropbox/GNA/SEKVENSER/Data/Sekvensobjekt level.Rda")
load("~/Dropbox/GNA/SEKVENSER/Data/Sekvensobjekt sektor.Rda")
library(TraMineR)
library(ggthemes)

stair.seq <- seq.level2_raw
fill.seq <- seq.sec_raw


filled.stairs <- function(stair.seq, fill.seq, reference = length(alphabet(stair.seq)), ncol = 4, hline = TRUE, colored = "nothing", time.values = 1:ncol(stair.seq), facet = NULL, base_size = 8){
  
  id.vars      <- "name"
  
  to.melt      <- function(x, id.vars, facet = NULL){
  xs           <- data.frame(name = rownames(x), x)
  
  if (is.null(facet) == FALSE){
    xs$facet <- facet
    id.vars     <- c(id.vars, "facet")
  }
  
  ms           <- melt(xs, id.vars = id.vars)
  
  ms$time      <- ms$variable
  
  ms$org.value <- ms$value
  ms$value     <- factor(ms$value, levels = alphabet(x), ordered = TRUE)
  ms$level     <- as.numeric(ms$value)
  ms
  }
  
  ms           <- to.melt(stair.seq, id.vars, facet)
  ms.fill      <- to.melt(fill.seq, id.vars, facet) 
  ms$fill      <- ms.fill$org.value
  
  ms$time      <- ms$variable
  levels(ms$time) <- time.values
  ms$time      <- as.numeric(as.character(ms$time))
  
  ms           <- ms %>% group_by(name) %>% mutate(first.reference = level %in% reference  & !duplicated(level %in% reference & is.na(level) == FALSE))
  ms$reference <- ms$level
  ms$reference[ms$first.reference == FALSE] <- NA 
  
  
  ms$colored   <- ms$value
  levels(ms$colored)[!levels(ms$colored) %in% colored] <- "Level"
  
  color.names  <- c("Level", colored)
  color.scale  <- c("black", grey.colors(n = length(colored), start = 0.75, end = 0.4))
  names(color.scale) <- color.names
  
  p    <- ggplot(data = ms, aes(x = time, y = level, group = name, color = colored, fill = fill)) 
  p    <- p + geom_bar(stat = "identity", color = NA, width = 1)
  p    <- p + geom_step(size = 0.2) + geom_point(aes(y = reference), size = 0.7, shape = 21, fill = "white")
  p    <- p + scale_color_manual(values = color.scale)
  p    <- p + facet_wrap(~name, strip.position = "left", dir = "v", ncol = ncol)
  
  if (is.null(facet) == FALSE) p    <- p + facet_wrap(~ facet + name, ncol = 2, strip.position = "left", dir = "v")
  
  p    <- p + scale_y_discrete(expand = c(0.1, 0.1)) + xlab("") + ylab("") + theme_tufte(base_size = base_size) + theme(legend.position = "none")
  p    <- p  + theme(strip.text.y = element_text(angle = 180, hjust = 1, size = base_size + 1,
                                                 margin = margin(0, 0, 0, 0, "cm")),
                     panel.spacing.y = unit(x = 0, units = "lines"), panel.spacing.x = unit(x = 0, units = "lines") )
  
  if (hline == TRUE) p <- p + geom_hline(yintercept = -0.5, size = 0.05, color = "grey50")
  
  p + scale_x_continuous(expand = c(0, 0))
}

filled.stairs(stair.seq, fill.seq, time.values = time.values, base_size = 6, reference = 5) + theme(strip.text = element_blank())

filled.stairs(stair.seq[1:50,], fill.seq[1:50,], time.values = time.values, base_size = 6, reference = 5)
