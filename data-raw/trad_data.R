trad_database  <- read.csv("tests/translation_file.csv", sep = ";")

Encoding(trad_database$fr) <- "UTF-8"

trad_database$fr <- iconv(
trad_database$fr,
  "UTF-8",
  "UTF-8"
)
Encoding(trad_database$es) <- "UTF-8"

trad_database$es <- iconv(
  trad_database$es,
  "UTF-8",
  "UTF-8"
)

usethis::use_data(trad_database, overwrite = TRUE)

