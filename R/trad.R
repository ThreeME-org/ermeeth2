#' Traduction à partir d'une base de données de langue
#'
#' @param x a character
#' @param lang translation destination language
#' @param data a R data frame
#' @param lang_source translation source language
#'
#' @import tidyr
#' @return a character vector
#' @export
#'
#' @examples \dontrun{trad("Alternatives", lang = "fr")}

trad <- function(x,data = trad_database,
                 lang = language, lang_source = "en"){

  # trad_database <- NULL
  # language <- NULL
  dest <- NULL

  if(exists("language") == FALSE){

    paste0("Please set translation destination language.") %>%
      message_warning()
  }

  trad_data <- data

  to_trad <- data.frame(source = x , dest=x) |>
    mutate(!! lang_source := source) |>
    left_join(trad_data, by = lang_source )

  to_trad[,"dest"] <- to_trad[,lang]

  res  <- to_trad |> mutate(dest = ifelse(is.na(dest),source ,dest) ) |> select(dest) |> as.vector() |> unlist() |> unname()

  res



}

# lang = "fr"
# lang_source = "en"
# data = trad_database
#
# x = c("World demand increase","Euro permanent depreciation","World demand increase", "plop")
#
# trad_data <- data


