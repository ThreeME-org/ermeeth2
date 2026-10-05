## Run with the package loaded: pkgload::load_all(); source("data-raw/dictionary.R")

## Builds `threeme_dictionary_data` from the plain-text source of truth.
##
## data-raw/dictionary.csv is the file to EDIT. It is a CSV, not an xlsx, so that
## changes diff and review in a pull request. Run this script after editing.
## documentation/improving_the_dictionary.qmd says what goes where.

threeme_dictionary_data <- utils::read.csv(
  "data-raw/dictionary.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  na.strings = c("", "NA")
)

## What the survey of the .mdl sources knows: a full label for the variables
## whose equation has a `##!` line, its French translation (not reviewed), and
## the indices. See data-raw/dictionary_survey.R.
survey <- utils::read.csv("data-raw/dictionary_survey.csv", stringsAsFactors = FALSE,
                          encoding = "UTF-8", na.strings = "NA")
survey <- survey[!is.na(survey$label_long_en), ]
from_survey <- threeme_dictionary_data[rep(NA_integer_, nrow(survey)), ]
from_survey$code <- survey$variable
from_survey$label_en <- survey$label_long_en
from_survey$label_fr <- survey$label_long_fr
from_survey$indexed_by <- vapply(strsplit(survey$index, ",", fixed = TRUE), function(i) {
  i <- intersect(c("sector", "commodity"), i)
  if (length(i)) paste(i, collapse = ",") else NA_character_
}, character(1))

## dictionary.csv wins, cell by cell: a line there only has to hold what it
## adds or corrects (a short label, a unit), the rest comes from the survey.
both <- intersect(threeme_dictionary_data$code, from_survey$code)
at_csv <- match(both, threeme_dictionary_data$code)
at_survey <- match(both, from_survey$code)
for (col in setdiff(names(threeme_dictionary_data), "code")) {
  empty <- is.na(threeme_dictionary_data[[col]][at_csv])
  threeme_dictionary_data[[col]][at_csv[empty]] <- from_survey[[col]][at_survey[empty]]
}
threeme_dictionary_data <- rbind(threeme_dictionary_data,
                                 from_survey[!from_survey$code %in% both, ])
threeme_dictionary_data <- threeme_dictionary_data[order(threeme_dictionary_data$code), ]
rownames(threeme_dictionary_data) <- NULL

stopifnot(
  !anyDuplicated(threeme_dictionary_data$code),
  all(dictionary_columns() %in% names(threeme_dictionary_data))
)

usethis::use_data(threeme_dictionary_data, overwrite = TRUE)
