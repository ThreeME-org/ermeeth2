## Console output goes through cli. Everything below is a message (stderr), so
## `suppressMessages()` silences it, and the text is passed to cli as a value,
## never as a template: braces in paths or equations are printed as they are.

#' Basic formatting function for display messages
#'
#' @description Prints `text_message` on a coloured background, preceded by
#'   `symbol_u`. Kept for backward compatibility: the `message_*()` helpers
#'   below no longer go through it.
#'
#' @param text_message  character string for the text to display
#' @param colour_bg character string colour of the background
#' @param text_black boolean, TRUE to keep the text in black, FALSE to make it white
#' @param symbol_u unicode symbol to display before the message
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_3me <- function(text_message, colour_bg = "seagreen2", text_black = TRUE, symbol_u = "\U2705"){

  style <- cli::combine_ansi_styles(
    cli::make_ansi_style(colour_bg, bg = TRUE),
    cli::make_ansi_style(if (text_black) "black" else "white"))
  txt <- style(paste(text_message, collapse = " "))
  cli::cli_text("{symbol_u} {txt}")
}

#' Display a success message
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_ok <- function(txt_message = "TEST"){
  cli::cli_alert_success("{txt_message}")
}

#' Display a failure message
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_not_ok <- function(txt_message = "TEST"){
  cli::cli_alert_danger("{txt_message}")
}

#' Display a warning message
#'
#' @description Informative only: no R warning is raised. Use
#'   [cli::cli_warn()] when the caller should be able to catch it.
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_warning <- function(txt_message = "TEST"){
  cli::cli_alert_warning("{txt_message}")
}

#' Display a stop message
#'
#' @description Prints the message only; it does not stop. Use
#'   [cli::cli_abort()] to signal the error itself.
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_stopbomb <- function(txt_message = "TEST"){
  cli::cli_alert_danger("{.strong {txt_message}}")
}

#' Display a saving message
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_save <- function(txt_message = "TEST"){
  cli::cli_alert("{txt_message}")
}

#' Display a deletion message
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_delete <- function(txt_message = "TEST"){
  cli::cli_alert("{txt_message}")
}

#' Display an informative message
#'
#' @param txt_message text to display
#' @param custom_symbol symbol to display before the text. The default, `""`,
#'   uses cli's info bullet.
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_any <- function(txt_message = "TEST", custom_symbol = ""){
  if (identical(custom_symbol, "")) {
    cli::cli_alert_info("{txt_message}")
  } else {
    cli::cli_text("{custom_symbol} {txt_message}")
  }
}

#' Display a main step heading
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_main_step <- function(txt_message = "TEST"){
  cli::cli_h2("{txt_message}")
}

#' Display a sub-step
#'
#' @param txt_message text to display
#'
#' @return Called for its side effect: a message.
#' @export
#'
message_sub_step <- function(txt_message = "TEST"){
  ## Not cli_progress_step(): in logs and knitr output (no dynamic terminal) any
  ## message printed while a step is open gets glued onto the step's line.
  cli::cli_alert_info("{txt_message}")
}

## Print a whole vector, wrapped to the console width. cli's inline vectors
## stop at 20 items, which is wrong for diagnostic lists (missing variables,
## unidentified codes) where every name matters.
cli_vector <- function(x, sep = "  ") {
  x <- as.character(x)
  if (length(x) == 0) return(invisible())
  lines <- utils::capture.output(
    cat(paste0(" ", x), sep = sep, fill = cli::console_width()))
  cli::cli_verbatim(lines)
}

## Make a string safe to pass to cli as a template.
cli_escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
