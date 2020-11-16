test.perfect.overlap <- function(){
# No merge ----
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[2])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[3])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s <-eliter::: merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A", "B", "C"))

# A and B are merged - because B contains A
# Label should be B & A - because B is the larger and A is merged into the larger.
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1:2)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[2])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[3])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- eliter:::merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A", "B & A", "C"))

# A and B are merged - because A and B are equal
# Label should be A & B - because A is first in the order.
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1:2)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[1:2])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[3])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- eliter:::merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A & B", "B", "C"))

# A, B and C are merged in the reverse order
# The labels should be C & B & A

el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(3)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[2:3])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[1:3])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A", "B", "C & A & B"))

# A and B are merged and A and C?
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1:2)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[1:3])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[2:3])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A", "B & A & C", "C"))

# A and C are the same, while B is smaller
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1,3)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[1:3])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[c(1,3)])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- eliter:::merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A & B & C", "B", "C"))

# A and C are the same, while B is smaller
el            <- list()
el$boris      <- tibble(NAME = "Boris", AFFILIATION = LETTERS[c(1,3)])
el$david      <- tibble(NAME = "David", AFFILIATION = LETTERS[1:3])
el$tony       <- tibble(NAME = "Tony",  AFFILIATION = LETTERS[c(1,3)])
el.theo       <- bind_rows(el)

incidence   <- xtabs(~NAME + AFFILIATION, el.theo, sparse = TRUE)
s             <- eliter:::merge.perfect.overlap(incidence) %>% colnames(incidence)
all.equal(s, c("A & B & C", "B", "C"))
}
