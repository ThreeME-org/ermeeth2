#' Find the year a shock starts
#'
#' @description The first year a non-baseline scenario departs from its
#'   reference. [table_3me()] uses it to anchor its `t`, `t + 1`, ... columns,
#'   which saves the caller from setting the `shockyear` global.
#'
#' @param data a ThreeME long-format dataframe.
#' @param name_baseline character(1) name of the baseline scenario.
#'
#' @returns numeric(1). The first year of the data if no scenario ever departs
#'   from its reference, or if the data carries no `values_ref` column.
#' @export
#'
#' @examples
#' data_full <- readRDS(system.file("extdata", "minimodel.rds", package = "ermeeth2"))
#' detect_shock_year(data_full)
detect_shock_year <- function(data, name_baseline = "baseline") {
  if (!"values_ref" %in% names(data)) return(min(data$year, na.rm = TRUE))
  shocked <- data[data$scenario != name_baseline, , drop = FALSE]
  diverged <- shocked$year[
    !is.na(shocked$values) & !is.na(shocked$values_ref) &
      shocked$values != shocked$values_ref
  ]
  if (length(diverged) == 0) min(data$year, na.rm = TRUE) else min(diverged)
}

## Turn `variables` - a character vector or a named list of groups - into a
## dataframe of variable / group / order, preserving the order given.
resolve_variable_groups <- function(variables) {
  if (is.list(variables)) {
    groups <- rep(names(variables), lengths(variables))
    codes <- unlist(variables, use.names = FALSE)
  } else {
    groups <- rep(NA_character_, length(variables))
    codes <- variables
  }
  data.frame(
    variable = as.character(codes),
    group = groups,
    .order = seq_along(codes),
    stringsAsFactors = FALSE
  )
}

## The years shown, and their "t + h" labels. The last column is always the
## final year available, labelled "Long term".
resolve_horizons <- function(horizons, shock_year, end_year, long_term_label = "Long term") {
  wanted <- shock_year + horizons
  wanted <- wanted[wanted <= end_year]
  years <- unique(c(wanted, end_year))
  labels <- ifelse(
    years == end_year, long_term_label,
    ifelse(years == shock_year, "t", paste0("t + ", years - shock_year))
  )
  data.frame(year = years, period = labels, stringsAsFactors = FALSE)
}

#' ThreeME results table
#'
#' @description A `gt` table of ThreeME results: variables in rows, time
#'   horizons in columns, scenarios as column spanners. Each variable can carry
#'   its own transformation - GDP in relative difference, employment in
#'   absolute difference, the unemployment rate in percentage points - with the
#'   unit shown in a column next to the variable name.
#'
#'   This is the single table entry point of the package: it replaces the former
#'   `table_macro()`, `table_macro2()`, `table_macro_double()`,
#'   `table_reference()` and `table_reference2()`. Passing `data_secondary`
#'   covers what `table_macro_double()` did: comparing the same scenarios across
#'   two versions of the model.
#'
#' @param data a ThreeME long-format dataframe, as produced by [longer_data()].
#' @param variables either a character vector of variable codes, or a named list
#'   of such vectors to organise the rows into groups - the list names become
#'   the row group labels.
#' @param transformation a single transformation id applied to every variable,
#'   or a character vector named by variable to mix them in one table. An
#'   unnamed element is the default for the variables it does not name. See
#'   [threeme_transformations()].
#' @param scenarios character vector of the scenarios of `data` to show.
#'   Defaults to every non-baseline scenario.
#' @param data_secondary an optional second ThreeME dataframe, typically the
#'   same scenarios run under another version of the model. Its columns are
#'   shown next to those of `data` under a second spanner level.
#' @param scenarios_secondary character vector of the scenarios to take from
#'   `data_secondary`. Defaults to `scenarios`.
#' @param model_names character vector of length 2 naming the two databases in
#'   the spanners.
#' @param horizons numeric vector of horizons, in years after the shock. The
#'   final year available is always added as a "Long term" column.
#' @param shock_year numeric(1) year the shock starts. `NULL` (default) detects
#'   it as the first year a scenario departs from its reference.
#' @param end_year numeric(1) last year shown. Defaults to the last year of the
#'   data.
#' @param labels named character vector mapping variable codes to display
#'   labels. Codes it does not cover keep their code. This is where the ThreeME
#'   variable dictionary will plug in once it exists.
#' @param name_baseline character(1) name of the baseline scenario.
#' @param base_year numeric(1) base year of the `"index100"` transformation.
#' @param digits numeric(1) decimals shown. Two by default, for both percentages
#'   and levels.
#' @param title,subtitle character(1) table header.
#' @param caption character(1) table caption, for cross-referencing.
#' @param footnote character(1) footnote. `NULL` (default) builds one describing
#'   the transformations used.
#' @param theme character(1) `"ofce"` to apply [ofce::theme.gt_ofce()],
#'   `"none"` to return a plain `gt` table.
#'
#' @returns A `gt_tbl`.
#' @importFrom rlang .data
#' @export
#'
#' @examples \dontrun{
#' table_3me(data_full, c("GDP", "CH", "I"))
#' table_3me(
#'   data_full,
#'   variables = list(Activity = c("GDP", "CH", "I"), Labour = c("UNR")),
#'   transformation = c(UNR = "ppdiff", "reldiff")
#' )
#' }
table_3me <- function(data,
                      variables,
                      transformation = "reldiff",
                      scenarios = NULL,
                      data_secondary = NULL,
                      scenarios_secondary = NULL,
                      model_names = c("model 1", "model 2"),
                      horizons = c(0, 1, 2, 5, 10),
                      shock_year = NULL,
                      end_year = NULL,
                      labels = NULL,
                      name_baseline = "baseline",
                      base_year = NULL,
                      digits = 2,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      footnote = NULL,
                      theme = c("ofce", "none")) {

  theme <- match.arg(theme)
  two_models <- !is.null(data_secondary)

  spec_vars <- resolve_variable_groups(variables)
  codes <- spec_vars$variable

  missing_vars <- setdiff(codes, unique(data$variable))
  if (length(missing_vars) == length(codes)) {
    stop(paste("None of these variables are in the data:", paste(codes, collapse = ", ")))
  }
  if (length(missing_vars) > 0) {
    warning(paste("Variables not found in the data, dropped:", paste(missing_vars, collapse = ", ")))
    spec_vars <- spec_vars[!spec_vars$variable %in% missing_vars, , drop = FALSE]
    codes <- spec_vars$variable
  }

  if (is.null(scenarios)) scenarios <- setdiff(unique(data$scenario), name_baseline)
  if (is.null(scenarios_secondary)) scenarios_secondary <- scenarios

  ## One frame per database, tagged with its model name.
  prepare <- function(d, scen, model) {
    out <- d |>
      dplyr::filter(.data$variable %in% codes, .data$scenario %in% c(scen, name_baseline))
    if (nrow(out) == 0) {
      stop(paste0("No rows left for the model '", model, "'. Check `variables` and `scenarios`."))
    }
    out$model <- model
    out
  }

  frames <- list(prepare(data, scenarios, model_names[1]))
  if (two_models) {
    frames[[2]] <- prepare(data_secondary, scenarios_secondary, model_names[2])
  }
  pooled <- dplyr::bind_rows(frames)

  if (is.null(shock_year)) shock_year <- detect_shock_year(pooled, name_baseline)
  if (is.null(end_year)) end_year <- max(pooled$year, na.rm = TRUE)

  periods <- resolve_horizons(horizons, shock_year, end_year)

  transformed <- threeme_transform(
    pooled,
    transformation = transformation,
    name_baseline = name_baseline,
    base_year = base_year
  )

  ## Percentage-style transformations are held as shares; display them in
  ## percent (or percentage points) so every column reads on the same scale.
  pct <- vapply(transformed$transformation, function(x) transformation_spec(x)$percent, logical(1))
  transformed$value[pct] <- transformed$value[pct] * 100

  body <- transformed |>
    dplyr::inner_join(periods, by = "year") |>
    dplyr::inner_join(spec_vars, by = "variable")

  if (nrow(body) == 0) {
    stop("No rows left after selecting the horizons. Check `horizons`, `shock_year` and `end_year`.")
  }

  label_map <- pretty_labels(codes, labels)
  body$label <- unname(label_map[body$variable])
  body <- with_sector_labels(body)

  ## One column per (model, scenario, period), laid out in the order the
  ## models, scenarios and horizons were asked for. The column names are
  ## opaque ids so that scenario names are free to contain anything.
  key <- expand.grid(
    period = periods$period,
    scenario = unique(body$scenario),
    model = model_names[seq_len(1 + two_models)],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  key <- key[, c("model", "scenario", "period")]
  key$col <- paste0("v", seq_len(nrow(key)))

  body <- dplyr::inner_join(body, key, by = c("model", "scenario", "period"))
  key <- key[key$col %in% body$col, , drop = FALSE]

  wide <- body |>
    dplyr::select(dplyr::all_of(c(".order", "group", "label", "unit", "col", "value"))) |>
    tidyr::pivot_wider(names_from = "col", values_from = "value") |>
    dplyr::arrange(.data$.order) |>
    dplyr::select(-".order")

  has_groups <- any(!is.na(wide$group))
  present <- intersect(key$col, names(wide))
  key <- key[match(present, key$col), , drop = FALSE]
  wide <- wide[, c(if (has_groups) "group", "label", "unit", key$col)]

  tbl <- gt::gt(
    wide,
    rowname_col = "label",
    groupname_col = if (has_groups) "group" else NULL,
    caption = caption
  ) |>
    gt::fmt_number(columns = dplyr::all_of(key$col), decimals = digits) |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_label(unit = "") |>
    gt::cols_align(align = "center", columns = "unit") |>
    gt::cols_label(.list = stats::setNames(as.list(key$period), key$col))

  ## One spanner per scenario, and when two databases are compared, one more
  ## level above them naming the model.
  for (m in unique(key$model)) {
    for (scen in unique(key$scenario)) {
      cols <- key$col[key$scenario == scen & key$model == m]
      if (length(cols) == 0) next
      tbl <- gt::tab_spanner(tbl, label = scen, columns = dplyr::all_of(cols),
                             id = paste(m, scen), level = 1)
    }
  }
  if (two_models) {
    for (m in unique(key$model)) {
      cols <- key$col[key$model == m]
      if (length(cols) == 0) next
      tbl <- gt::tab_spanner(tbl, label = m, columns = dplyr::all_of(cols),
                             id = paste0("model-", m), level = 2)
    }
  }

  if (!is.null(title) || !is.null(subtitle)) {
    tbl <- gt::tab_header(tbl, title = title, subtitle = subtitle)
  }

  if (is.null(footnote)) {
    used <- unique(body$transformation)
    parts <- vapply(used, function(x) {
      s <- transformation_spec(x)
      if (nzchar(s$symbol)) paste0(s$symbol, " : ", s$label) else s$label
    }, character(1))
    footnote <- paste(parts, collapse = ", ")
  }
  if (nzchar(footnote)) tbl <- gt::tab_source_note(tbl, footnote)

  if (theme == "ofce") tbl <- ofce::theme.gt_ofce(tbl)

  tbl
}
