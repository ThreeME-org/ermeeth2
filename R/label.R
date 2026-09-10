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

  cli::cli_warn(c("{.fn label} is deprecated: use {.fn dict_label}, which reads {.fn threeme_dictionary}.",
                  "i" = "The {.arg data} argument is ignored."), call = NULL)

  if (!lang %in% dictionary_languages()) {
    cli::cli_abort(c("{.code lang = \"{lang}\"} is not carried by the dictionary.",
                     "i" = "Available: {.val {dictionary_languages()}}."))
  }
  dict_label(variable_code, lang = lang)
}
