## Deprecated wrappers -----------------------------------------------------
##
## teXdoc() parsed the .mdl files with a `pegr` grammar and wrote a .tex,
## make_eq_qmd() then scraped that .tex with regular expressions to produce
## the .qmd files. Both are replaced by model_doc(), which parses once and
## renders each format from the parsed document. See PLAN.md for the list of
## bugs that motivated the rewrite.

#' teXdoc takes a model file
#'
#' @description Deprecated. Use [model_doc()] with
#'   `format = "tex"`. This wrapper forwards to it so existing scripts keep
#'   running; it no longer writes the `<out>_preface.tex` companion file,
#'   which only existed to feed [make_eq_qmd()].
#'
#' @param sources mdl source files containing the equations
#' @param exo mdl source files containing the exogenous variables
#' @param base.path source files directory
#' @param out output file name (no directory: see `out.path`)
#' @param out.path output files directory (default is the working directory)
#' @param compile_pdf `TRUE` compiles the tex file into a pdf
#'
#' @returns invisibly, the value of [model_doc()].
#' @export
#'
#' @examples
#' \dontrun{
#'  teXdoc(sources   = c("model_test.mdl"),
#'         exo       = c("model_test-exovar.mdl"),
#'         base.path = system.file(package = "ermeeth2"),
#'         out       = "documentation-eq",
#'         out.path  = "documentation_test")
#' }
teXdoc <- function(sources, exo = c(), base.path = "src/model", out = "doc",
                   out.path = getwd(), compile_pdf = FALSE) {
  warning("`teXdoc()` is deprecated: use `model_doc(format = \"tex\")`, ",
          "or `model_doc()` for Quarto output.", call. = FALSE)
  invisible(model_doc(sources = sources, exo = exo, base.path = base.path,
                      out = out, out.path = out.path, format = "tex",
                      compile_pdf = compile_pdf))
}

#' Transform the tex equation files into qmd
#'
#' @description Deprecated. The Quarto output is no
#'   longer derived from the LaTeX output: call [model_doc()] on the `.mdl`
#'   sources instead, which produces one self-contained `.qmd`. This
#'   function now only raises an error pointing there, because it cannot
#'   recover the `.mdl` sources from the `.tex` files it used to read.
#'
#' @param preface,maintex paths to the tex files the old `teXdoc()` wrote.
#' @param path files path
#' @param out.dir output directory
#'
#' @returns nothing: always raises an error.
#' @export
make_eq_qmd <- function(preface = "03.1-eq_preface.tex",
                        maintex = "03.1-eq.tex",
                        path = file.path("results", "quarto_templates", "results_side_files"),
                        out.dir = file.path("results", "quarto_templates", "results_side_files")) {
  stop("`make_eq_qmd()` is removed. `model_doc()` writes the Quarto document ",
       "directly from the .mdl sources:\n",
       "  model_doc(sources = ..., exo = ..., base.path = ..., out.path = \"",
       out.dir, "\")", call. = FALSE)
}
