library(soc.elite)
data(den)
# den.db          <- eliteDB.connections(pass = "FEM2015")
# den.db          <- den.db[is.na(den.db$AFFILIATION) == FALSE,] 
# den.db          <- den.db[is.na(den.db$ARCHIVED),]
# den.db          <- droplevels(den.db)
# den             <- den.db
data(pe13)
#den             <- den[-which(den.db$AFFILIATION == "Den Berlingske Fond"),]

e2          <- ego.two.mode("Peter Gæmelke", den = den, member.of = pe13$Name)
e1          <- ego.two.mode("Peter Gæmelke", den = den, member.of = pe13$Name, n = 2)

cairo_pdf(filename = "~/My Dropbox/Elite og Tal/Udtræk/Personudtræk/Peter Gæmelke.pdf", onefile = TRUE, height = 10, width = 10)
e2
e1
dev.off()