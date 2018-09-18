# Themes ----

theme.elite <- function(...){
  
}

theme_fem <-  function (size = 11, font = "Libre Baskerville", face = "plain", backgroundColor = "white", 
                        panelColor = "white", axisColor = "white", gridColor = "white", 
                        textColor = "black") 
{
  theme(panel.border = element_rect(colour = gridColor, linetype = "solid", 
                                    fill = NA), axis.text.x = element_text(vjust = 1, hjust = 0.5, 
                                                                           colour = axisColor, family = font, face = face, size = 9), 
        axis.text.y = element_text(hjust = 1, vjust = 0.5, colour = axisColor, 
                                   family = font, face = face, size = 9),
        axis.title.x = element_text(family = font,face = face, colour = textColor, size = size),
        axis.title.y = element_text(angle = 90, family = font, face = face, colour = textColor, size = size), 
        
        axis.line = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA, colour = gridColor), 
        legend.key = element_blank(), legend.key.size = unit(1.5, 
                                                             "lines"), legend.text = element_text(hjust = 0, family = font, 
                                                                                                  face = face, colour = textColor, size = size), legend.title = element_text(hjust = 0, 
                                                                                                                                                                             family = font, face = face, colour = textColor, size = size), 
        panel.background = element_rect(fill = panelColor, colour = NA), 
        plot.background = element_rect(fill = backgroundColor, 
                                       colour = NA), panel.grid.major = element_line(colour = gridColor, 
                                                                                     size = 0.33, linetype = "dotted"), panel.grid.minor = element_blank(), 
        strip.background = element_rect(fill = NA, colour = NA), 
        strip.text.x = element_text(hjust = 0, family = font, 
                                    face = face, colour = textColor, size = size), strip.text.y = element_text(angle = -90, 
                                                                                                               family = font, face = face, colour = textColor, size = size), 
        plot.title = element_text(hjust = 0, vjust = 1, family = font, 
                                  face = face, colour = textColor, size = 15), plot.margin = unit(c(0.3, 
                                                                                                    0.1, 0.1, 0.1), "lines"))
}
