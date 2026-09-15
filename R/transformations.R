#' ThreeME data transformations
#'
#' @description The registry of the ways a ThreeME variable can be looked at.
#'   Both [simple_plot()] and [table_3me()] take their `transformation`
#'   argument from this list, so a transformation only has to be described once.
#'
#' Each entry carries:
#' \describe{
#'   \item{label}{a human-readable description, used in footnotes and axis labels}
#'   \item{symbol}{the short unit marker shown in the table's unit column}
#'   \item{needs_ref}{whether the transformation requires a `values_ref` column}
#'   \item{drop_baseline}{whether the baseline rows are meaningless (all zero)
#'     and should be dropped}
#'   \item{percent}{whether the result is a share, to be displayed multiplied by
#'     100 with a percent-style formatter}
#' }
#'
#' @returns A named list of transformation specifications.
#' @export
#'
#' @examples
#' names(threeme_transformations())
threeme_transformations <- function() {
  list(
    level = list(
      id = "level", label = "level", symbol = "",
      needs_ref = FALSE, drop_baseline = FALSE, percent = FALSE
    ),
    reldiff = list(
      id = "reldiff", label = "relative difference to the baseline", symbol = "%",
      needs_ref = TRUE, drop_baseline = TRUE, percent = TRUE
    ),
    diff = list(
      id = "diff", label = "absolute difference to the baseline", symbol = "Δ",
      needs_ref = TRUE, drop_baseline = TRUE, percent = FALSE
    ),
    ppdiff = list(
      id = "ppdiff", label = "difference in percentage points", symbol = "pp",
      needs_ref = TRUE, drop_baseline = TRUE, percent = TRUE
    ),
    gr = list(
      id = "gr", label = "annual growth rate", symbol = "↗",
      needs_ref = FALSE, drop_baseline = FALSE, percent = TRUE
    ),
    index100 = list(
      id = "index100", label = "index (base year = 100)", symbol = "t₀=100",
      needs_ref = FALSE, drop_baseline = FALSE, percent = FALSE
    )
  )
}

#' Look up one transformation specification
#'
#' @param transformation character(1) the transformation id.
#' @returns The matching element of [threeme_transformations()].
#' @export
transformation_spec <- function(transformation) {
  specs <- threeme_transformations()
  transformation <- match.arg(transformation, names(specs))
  specs[[transformation]]
}

## Columns identifying a single series, used to group lagged transformations.
series_keys <- function(data) {
  intersect(c("variable", "scenario", "sector", "commodity", "model"), names(data))
}

## Resolve a transformation argument into one value per variable present in data.
resolve_transformation <- function(data, transformation) {
  specs <- names(threeme_transformations())
  variables <- unique(data$variable)

  if (is.null(names(transformation))) {
    if (length(transformation) != 1) {
      stop("`transformation` must be a single value, or a vector named by variable.")
    }
    transformation <- match.arg(transformation, specs)
    return(stats::setNames(rep(transformation, length(variables)), variables))
  }

  unknown <- setdiff(transformation, specs)
  if (length(unknown) > 0) {
    stop(paste0(
      "Unknown transformation(s): ", paste(unique(unknown), collapse = ", "),
      ". Available: ", paste(specs, collapse = ", "), "."
    ))
  }

  ## An unnamed element acts as the default for the variables not listed.
  default <- transformation[names(transformation) == ""]
  named <- transformation[names(transformation) != ""]
  default <- if (length(default) > 0) default[[1]] else "reldiff"

  out <- stats::setNames(rep(default, length(variables)), variables)
  common <- intersect(names(named), variables)
  out[common] <- named[common]
  out
}

#' Apply a ThreeME transformation to long-format data
#'
#' @description Adds a `value` column holding the transformed series, plus
#'   `transformation` and `unit` columns describing it. This is the single
#'   place where "relative difference", "growth rate" and friends are defined;
#'   [simple_plot()] and [table_3me()] both go through it.
#'
#' @param data a ThreeME long-format dataframe, with at least `year`,
#'   `variable`, `scenario` and `values` (plus `values_ref` for the
#'   transformations comparing to a reference scenario).
#' @param transformation either a single transformation id applied to every
#'   variable, or a character vector named by variable to mix transformations
#'   in one call. An unnamed element in that vector is used as the default for
#'   the variables it does not name. See [threeme_transformations()].
#' @param name_baseline character(1) name of the baseline scenario. Its rows are
#'   dropped for the transformations expressed relative to it, where they would
#'   be identically zero.
#' @param base_year numeric(1) base year of the `"index100"` transformation.
#'   Defaults to the first year of `data`.
#'
#' @returns `data` with the added `value`, `transformation` and `unit` columns.
#'   Rows may be dropped (baseline rows for difference transformations); the
#'   first year of each series is `NA` for `"gr"`.
#' @export
#'
#' @examples \dontrun{
#' threeme_transform(data_full, "reldiff")
#' threeme_transform(data_full, c(GDP = "reldiff", UNR = "ppdiff", "level"))
#' }
threeme_transform <- function(data,
                              transformation = "reldiff",
                              name_baseline = "baseline",
                              base_year = NULL) {

  values <- NULL
  values_ref <- NULL
  variable <- NULL
  year <- NULL

  required <- c("year", "variable", "scenario", "values")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("The data is missing the column(s):", paste(missing_cols, collapse = ", ")))
  }

  per_variable <- resolve_transformation(data, transformation)
  if (is.null(base_year)) base_year <- min(data$year, na.rm = TRUE)

  needs_ref <- vapply(unique(per_variable), function(x) transformation_spec(x)$needs_ref, logical(1))
  if (any(needs_ref) && !"values_ref" %in% names(data)) {
    stop(paste0(
      "The transformation(s) ", paste(unique(per_variable)[needs_ref], collapse = ", "),
      " require a 'values_ref' column."
    ))
  }

  keys <- series_keys(data)

  pieces <- lapply(unique(per_variable), function(tr) {
    spec <- transformation_spec(tr)
    d <- dplyr::filter(data, variable %in% names(per_variable)[per_variable == tr])
    if (nrow(d) == 0) return(NULL)

    d <- switch(
      tr,
      level    = dplyr::mutate(d, value = values),
      reldiff  = dplyr::mutate(d, value = values / values_ref - 1),
      diff     = dplyr::mutate(d, value = values - values_ref),
      ppdiff   = dplyr::mutate(d, value = values - values_ref),
      gr       = d |>
        dplyr::arrange(year) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::mutate(value = values / dplyr::lag(values) - 1) |>
        dplyr::ungroup(),
      index100 = d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::mutate(value = values / values[year == base_year][1] * 100) |>
        dplyr::ungroup()
    )

    if (spec$drop_baseline) d <- dplyr::filter(d, scenario != name_baseline)

    d$transformation <- tr
    d$unit <- spec$symbol
    d
  })

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    stop("No rows left after transformation. Check `variables`, `scenarios` and `name_baseline`.")
  }
  as.data.frame(out)
}
