#' Label variable codes (deprecated)
#'
#' Superseded by [dict_label()], which resolves a code to a label in one lookup
#' against [threeme_dictionary()] instead of going through `label_database` and
#' then [trad()], and which takes its dictionary as an argument rather than
#' reading a global.
#'
#' @param variable_code a character or a character vector.
#' @param data a R data frame. Ignored; the dictionary is used instead.
#' @param lang label destination language.
#'
#' @returns a character or a character vector.
#' @export
#'
#' @examples \dontrun{label("GDP")}
label <- function(variable_code,
                  data = NULL,
                  lang = "en"){

  warning("`label()` is deprecated: use `dict_label()`, which reads ",
          "`threeme_dictionary()`. The `data` argument is ignored.",
          call. = FALSE)

  if (!lang %in% dictionary_languages()) {
    stop("`lang = \"", lang, "\"` is not carried by the dictionary. ",
         "Available: ", paste(dictionary_languages(), collapse = ", "), ".")
  }
  dict_label(variable_code, lang = lang)
}
