# Ancestry ----

# An ancestry graph is a graph which only contains directed edges between CEOs and the chairmen who elected them.

library(eliter)
library(readr)

den.db      <- read_delim(file = "~/Dropbox/GNA/Til DST/den_cvr.csv", delim = ",")
