#' Render the configured quarto reports
#'
#' @description Renders each selected quarto template with its parameters via
#'   [render_from_template()], and optionally opens the results.
#'
#' @details Two entries of a template's parameter list are render options, not
#'   template parameters, and are not passed on to the template:
#'
#'   - `format`: `"html"`, `"revealjs"` or `"typst"`. Without it, the format
#'     follows the template's `pres` parameter: `revealjs` when `pres` is
#'     `TRUE` (the default of the presentation templates), `html` otherwise.
#'   - `keep_qmd`: `TRUE` keeps the copy of the template written at the root of
#'     the project, as `<template>_<project_name>_<date>.qmd`, with the default
#'     values of its `params` replaced by the ones of the configuration. That
#'     copy renders on its own, so it can be edited and rendered again to
#'     adjust a report: the Render button renders it to the format it was made
#'     for (listed first), `quarto render` on the command line to every format
#'     it lists. Overrides the `keep_qmd` argument for that template.
#'
#' @param Show logical. Whether to open each rendered report when it is done.
#' @param templates_selection named list of logicals. The templates to render,
#'   by name (without the `.qmd` extension).
#' @param parameters named list. Quarto parameters, one element per template.
#' @param templates_path character. Folder holding the quarto templates.
#' @param output_path character. Folder the rendered reports land in.
#' @param project_name character. Project name, used to build output filenames.
#' @param keep_qmd logical. Whether to keep the root copy of every template
#'   (see Details).
#'
#' @returns Invisibly, the paths of the rendered reports, named by template.
#' @export
#' @examples
#' \dontrun{
#' ## in config_output_*.R
#' quartos_parameters <- list(
#'   standard_shocks = list(project_name = project_name,
#'                          format = "typst",     # instead of the slides
#'                          keep_qmd = TRUE)      # keep a copy to edit
#' )
#' produce_quartos()
#' }
produce_quartos <- function(Show = TRUE,
                            templates_selection = config$output$quartos_to_render,
                            parameters = config$output$quartos_parameters,
                            templates_path = file.path("results", "quarto_templates"),
                            output_path = file.path("results", "quarto_render"),
                            project_name = config$input$project_name,
                            keep_qmd = FALSE) {

  ## The defaults read `config` from the calling environment: no `<- NULL`
  ## for it here, that would hide it.
  quartos_to_produce <- names(templates_selection)[vapply(templates_selection, isTRUE, logical(1))]
  if (length(quartos_to_produce) == 0) {
    cli::cli_inform("No quarto to render.")
    return(invisible(character()))
  }

  stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  rendered <- purrr::map_chr(purrr::set_names(quartos_to_produce), function(template) {
    params <- parameters[[template]]
    if (is.null(params)) params <- list()
    keep <- if (is.null(params$keep_qmd)) keep_qmd else isTRUE(params$keep_qmd)
    render_from_template(quarto_to_use = stringr::str_c(template, ".qmd"),
                         quartos_folder = templates_path,
                         interim_file_name = stringr::str_c(template, "_", project_name, "_", stamp, ".qmd"),
                         output_file_name = stringr::str_c(project_name, "_", template, "_", stamp),
                         quarto_parameters_list = params[setdiff(names(params), c("format", "keep_qmd"))],
                         format = params$format,
                         render_folder = output_path,
                         keep_qmd = keep,
                         browse = FALSE)
  })

  if (Show) purrr::walk(rendered, utils::browseURL)

  invisible(rendered)
}
