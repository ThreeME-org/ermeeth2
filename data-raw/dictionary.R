## Run with the package loaded: pkgload::load_all(); source("data-raw/dictionary.R")

## Builds `threeme_dictionary_data` from the plain-text source of truth.
##
## data-raw/dictionary.csv is the file to EDIT. It is a CSV, not an xlsx, so that
## changes diff and review in a pull request. Run this script after editing.

threeme_dictionary_data <- utils::read.csv(
  "data-raw/dictionary.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  na.strings = c("", "NA")
)

stopifnot(
  !anyDuplicated(threeme_dictionary_data$code),
  all(dictionary_columns() %in% names(threeme_dictionary_data))
)

usethis::use_data(threeme_dictionary_data, overwrite = TRUE)
