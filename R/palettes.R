#' ThreeME colour palettes
#'
#' @description Returns a vector of colours, optionally named, for use as the
#'   `palette` argument of [simple_plot()] and [table_3me()]. The built-in
#'   palettes are the ones used in the `basic_results.qmd` template of the
#'   ThreeME v4 repository, plus the OFCE palette from the `ofce` package.
#'
#' @param n numeric(1) number of colours wanted. Ignored when `keys` is given.
#' @param keys character vector of names to attach to the colours (variable
#'   codes or scenario names). The palette is recycled if it is shorter.
#' @param palette character vector of colours overriding the built-in choice,
#'   or `NULL` (default). May itself be named, in which case the names are
#'   honoured for the `keys` they match and the remaining keys fall back to
#'   `type`.
#' @param type character(1) which built-in palette to use: `"distinct"` for
#'   qualitative series (variables), `"gradient"` for ordered ones (scenarios),
#'   or `"ofce"` for [ofce::ofce_palette()].
#'
#' @returns A character vector of hex colours, named when `keys` is supplied.
#' @export
#'
#' @examples
#' threeme_palette(n = 4)
#' threeme_palette(keys = c("GDP", "CH", "I"), type = "distinct")
threeme_palette <- function(n = NULL,
                            keys = NULL,
                            palette = NULL,
                            type = c("distinct", "gradient", "ofce")) {

  type <- match.arg(type)

  base <- switch(
    type,
    distinct = c("#CD5C5C", "#E9967A", "#FFEC9E", "#9BCD9B",
                 "#87CEFA", "#CD96CD", "#FFB6C1", "#8B7355"),
    gradient = c("#5D478B", "#27408B", "#1C86EE", "#20B2AA",
                 "#2E8B57", "#00688B", "#6E7B8B"),
    ofce     = ofce::ofce_palette(n = max(n %||% 2, length(keys), 2))
  )

  named_override <- character(0)
  if (!is.null(palette)) {
    if (is.null(names(palette))) {
      base <- palette
    } else {
      named_override <- palette
    }
  }

  if (is.null(keys)) {
    if (is.null(n)) stop("Give either `n` or `keys`.")
    return(rep_len(base, n))
  }

  out <- stats::setNames(rep_len(base, length(keys)), keys)
  common <- intersect(names(named_override), keys)
  out[common] <- named_override[common]
  out
}

## Default palette for ThreeME macro aggregates, from basic_results.qmd.
threeme_variable_palette <- function() {
  c(GDP = "#2B2D2DFF", Y = "#2B2D2DFF", CH = "#E9967A", M = "#CD5C5C",
    I = "#FFEC8B", G = "#9BCD9B", X = "#87CEFA")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

## Turn variable codes into display labels. `labels` is a named vector
## (code -> label); codes it does not name keep their code. The dictionary of
## ThreeME variables is not built yet - when it is, it plugs in here.
pretty_labels <- function(codes, labels = NULL) {
  out <- stats::setNames(as.character(codes), codes)
  if (is.null(labels)) return(out)
  if (is.null(names(labels))) {
    ## positional labels, as the old simple_plot() accepted
    if (length(labels) != length(codes)) {
      stop("Unnamed `labels` must have one entry per variable.")
    }
    return(stats::setNames(as.character(labels), codes))
  }
  common <- intersect(names(labels), codes)
  out[common] <- labels[common]
  out
}

## Append the sector / commodity to a label when the data carries one.
with_sector_labels <- function(data, label_col = "label") {
  sector <- NULL
  commodity <- NULL
  for (col in intersect(c("sector", "commodity"), names(data))) {
    has <- !is.na(data[[col]]) & nzchar(data[[col]])
    data[[label_col]][has] <- paste0(
      data[[label_col]][has], " - ", data[[col]][has]
    )
  }
  data
}
