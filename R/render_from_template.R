### Render quarto from template

#' Render a single quarto report from a template
#'
#' @description Copies a quarto template to the root of the project with the
#'   given parameters written in as its defaults, renders that copy, then
#'   removes it unless it is kept.
#'
#' @details The copy is self-contained: its `params` defaults are the values
#'   passed in `quarto_parameters_list` (written as `!expr`, so named vectors
#'   and lists keep their names), and its `format` block lists the format
#'   rendered first. Kept with `keep_qmd = TRUE`, it can be edited and rendered
#'   again on its own.
#'
#'   Without `format`, the format follows the `pres` parameter (given, or the
#'   template's default): `revealjs` when `TRUE`, `html` otherwise. When the
#'   template declares `pres`, it is set to match the format rendered.
#'
#' @param quarto_to_use character. Filename of the template to render.
#' @param quartos_folder character. Folder holding the quarto templates.
#' @param interim_file_name character. Name of the copy written at the root
#'   before rendering.
#' @param output_file_name character. Name of the rendered output file. The
#'   extension is added from the format when missing.
#' @param quarto_parameters_list list. Values of the template's parameters.
#' @param format character. `"html"`, `"revealjs"` or `"typst"`, or `NULL` to
#'   follow `pres`.
#' @param render_folder character. Folder the rendered output lands in (the
#'   `output-dir` of the quarto project).
#' @param keep_qmd logical. Whether to keep the copy after rendering.
#' @param browse logical. Whether to open the rendered file when it is done.
#'
#' @returns Invisibly, the path of the rendered report.
#' @export
render_from_template <- function(
    quarto_to_use = "basic_results.qmd",
    quartos_folder = file.path("results", "quarto_templates"),
    interim_file_name = "main_results.qmd",
    output_file_name = stringr::str_c("main_results_", format(Sys.time(), "%Y-%m-%d_%H-%M")),
    quarto_parameters_list = list(),
    format = NULL,
    render_folder = file.path("results", "quarto_render"),
    keep_qmd = FALSE,
    browse = TRUE) {

  ### 0 - function parameters check
  template_file <- file.path(quartos_folder, quarto_to_use)
  if (!file.exists(template_file)) {
    cli::cli_abort("Template not found: {.file {template_file}}")
  }
  if (tools::file_ext(interim_file_name) == "") interim_file_name <- stringr::str_c(interim_file_name, ".qmd")

  lines <- readLines(template_file, warn = FALSE)
  fm <- split_front_matter(lines, template_file)
  defaults <- fm$yaml$params
  if (is.null(defaults)) defaults <- list()
  quarto_parameters_list <- as.list(quarto_parameters_list)

  ### 1 - output format
  if (is.null(format)) {
    pres <- if ("pres" %in% names(quarto_parameters_list)) quarto_parameters_list$pres else defaults$pres
    format <- if (isTRUE(pres)) "revealjs" else "html"
  }
  format <- match.arg(format, c("html", "revealjs", "typst"))
  if ("pres" %in% names(defaults)) quarto_parameters_list$pres <- format == "revealjs"

  if (tools::file_ext(output_file_name) == "") {
    output_file_name <- stringr::str_c(output_file_name, if (format == "typst") ".pdf" else ".html")
  }

  ### 2 - copy the template, parameters and format written in
  header <- fm$header
  header <- replace_yaml_block(header, "params",
                               merge_params_block(yaml_block(header, "params"), quarto_parameters_list))
  header <- replace_yaml_block(header, "format",
                               first_format_block(yaml_block(header, "format"), format))
  writeLines(c("---", header, "---", fm$body), interim_file_name)
  if (!keep_qmd) on.exit(file.remove(interim_file_name), add = TRUE)

  ### 3 - render
  quarto::quarto_render(input = interim_file_name,
                        output_format = format,
                        output_file = output_file_name)

  if (keep_qmd) cli::cli_inform("Copy of the template kept: {.file {interim_file_name}}")

  output <- file.path(render_folder, output_file_name)

  ### 4 - browse output
  if (browse) utils::browseURL(output)

  invisible(output)
}

## Front matter of a qmd: the YAML lines between the first two `---`, parsed
## (an `!expr` value is kept as its code), and the rest of the file.
split_front_matter <- function(lines, file = "") {
  delim <- which(trimws(lines) == "---")
  if (length(delim) < 2 || delim[1] != 1) {
    cli::cli_abort("No YAML front matter in {.file {file}}")
  }
  header <- lines[(delim[1] + 1):(delim[2] - 1)]
  parsed <- yaml::yaml.load(paste(header, collapse = "\n"),
                            handlers = list(expr = function(x) structure(x, class = "yaml_expr")))
  list(header = header,
       yaml = parsed,
       body = if (delim[2] < length(lines)) lines[(delim[2] + 1):length(lines)] else character())
}

## The lines of a top-level YAML key: its own line and the indented lines (or
## blank / comment lines) that follow it. NULL when the key is absent.
yaml_block_range <- function(header, key) {
  start <- grep(stringr::str_c("^", key, "\\s*:"), header)
  if (length(start) == 0) return(NULL)
  start <- start[1]
  end <- start
  while (end < length(header) && grepl("^(\\s|#|$)", header[end + 1])) end <- end + 1
  ## trailing blank lines belong to no block
  while (end > start && trimws(header[end]) == "") end <- end - 1
  start:end
}

yaml_block <- function(header, key) {
  range <- yaml_block_range(header, key)
  if (is.null(range)) NULL else header[range]
}

replace_yaml_block <- function(header, key, block) {
  range <- yaml_block_range(header, key)
  if (is.null(range)) return(c(header, block))
  c(header[seq_len(range[1] - 1)], block, header[-seq_len(max(range))])
}

## The entries of a YAML block (its first line excluded), named by key, each
## with its continuation lines. A value on the key line (`format: html`) is
## returned as an entry of that name.
yaml_children <- function(block) {
  inline <- trimws(sub("^[^:]+:", "", block[1]))
  inline <- trimws(sub("#.*$", "", inline))
  if (inline != "") return(stats::setNames(list(stringr::str_c("  ", inline, ": default")), inline))
  rest <- block[-1]
  keys <- grepl("^\\s+[A-Za-z0-9_.-]+\\s*:", rest)
  if (!any(keys)) return(list())
  indent <- min(nchar(sub("\\S.*$", "", rest[keys])))
  starts <- which(keys & nchar(sub("\\S.*$", "", rest)) == indent)
  ends <- c(starts[-1] - 1, length(rest))
  ## re-indented by two spaces, as the entries this file adds
  shift <- function(x) {
    if (indent < 2) return(ifelse(grepl("^\\s", x), paste0(strrep(" ", 2 - indent), x), x))
    sub(paste0("^ {0,", indent - 2, "}"), "", x)
  }
  entries <- purrr::map2(starts, ends, ~ shift(rest[.x:.y]))
  stats::setNames(entries, trimws(sub(":.*$", "", rest[starts])))
}

## A parameter as a YAML entry: plain YAML for a single unnamed value, `!expr`
## and its R code otherwise.
param_yaml <- function(name, value) {
  if (is.null(value)) return(stringr::str_c("  ", name, ": null"))
  plain <- is.atomic(value) && length(value) == 1 && is.null(names(value)) &&
    !is.na(value) && (is.character(value) || is.numeric(value) || is.logical(value))
  if (plain) {
    scalar <- if (is.character(value)) {
      stringr::str_c("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", value)), "\"")
    } else if (is.logical(value)) {
      tolower(as.character(value))
    } else {
      format(value, scientific = FALSE, digits = 15)
    }
    return(stringr::str_c("  ", name, ": ", scalar))
  }
  code <- paste(deparse(value, width.cutoff = 500L), collapse = " ")
  stringr::str_c("  ", name, ": !expr '", gsub("'", "''", code), "'")
}

## The params block of the template with the given values in place of the
## defaults. Parameters the template does not declare are added at the end.
merge_params_block <- function(block, values) {
  entries <- if (is.null(block)) list() else yaml_children(block)
  for (name in names(values)) entries[[name]] <- param_yaml(name, values[[name]])
  if (length(entries) == 0) return(character())
  c("params:", unlist(entries, use.names = FALSE))
}

## The format block with the rendered format first, so that the copy renders
## to it by default.
first_format_block <- function(block, format) {
  entries <- if (is.null(block)) list() else yaml_children(block)
  if (is.null(entries[[format]])) entries[[format]] <- stringr::str_c("  ", format, ": default")
  entries <- c(entries[format], entries[setdiff(names(entries), format)])
  c("format:", unlist(entries, use.names = FALSE))
}
