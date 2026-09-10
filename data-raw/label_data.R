library(openxlsx)

label_database  <- read.xlsx("tests/label.xlsx")

usethis::use_data(label_database, overwrite = TRUE)
