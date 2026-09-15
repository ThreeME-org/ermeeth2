#' Plot ThreeME variables
#'
#' @description A line chart of a set of ThreeME variables, over one or several
#'   scenarios, in any of the transformations listed by
#'   [threeme_transformations()] and defaulting to the relative difference to
#'   the baseline. This is the single plotting entry point of the package: it
#'   replaces the former `simple_plot()` and `simpleplot()`.
#'
#'   With `interactive = TRUE` the lines gain small hover points carrying a
#'   tooltip with the scenario name, the level under the shock and under the
#'   baseline, the growth rate and the relative difference, and the plot is
#'   returned as a `girafe` object through [ofce::girafy()].
#'
#' @param data a ThreeME long-format dataframe, as produced by [longer_data()]:
#'   columns `year`, `variable`, `scenario`, `values` and `values_ref`, and
#'   optionally `sector` / `commodity`.
#' @param variables character vector of the variables to plot.
#' @param scenarios character vector of the scenarios to plot. Defaults to every
#'   scenario in `data`.
#' @param transformation character(1) one of the ids of
#'   [threeme_transformations()]. A plot carries a single y axis, so mixing
#'   transformations in one call is not allowed - use [table_3me()] for that.
#' @param labels named character vector mapping variable codes to display
#'   labels, or an unnamed vector with one entry per variable. Codes it does not
#'   cover keep their code. Defaults to the codes themselves.
#' @param startyear,endyear numeric(1) first and last year plotted. Default to
#'   the range of `data`.
#' @param base_year numeric(1) base year of the `"index100"` transformation.
#' @param name_baseline character(1) name of the baseline scenario.
#' @param palette character vector of colours, optionally named by variable or
#'   scenario, passed to [threeme_palette()].
#' @param colour_by character(1) `"variable"` or `"scenario"`: which dimension
#'   gets the colour, the other getting the linetype. Defaults to `"scenario"`
#'   when a single variable is plotted, `"variable"` otherwise.
#' @param title,subtitle,caption character(1) plot labels.
#' @param digits numeric(1) number of decimals on the y axis labels.
#' @param x_breaks numeric(1) spacing of the x axis ticks. `NULL` (default)
#'   picks a spacing from the length of the period.
#' @param interactive boolean(1) return an interactive `girafe` object instead
#'   of a plain ggplot.
#'
#' @returns A `ggplot`, or a `girafe` object when `interactive = TRUE`.
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
#' @examples \dontrun{
#' simple_plot(data_full, c("GDP", "CH", "I"))
#' simple_plot(data_full, "GDP", transformation = "gr", interactive = TRUE)
#' }
simple_plot <- function(data,
                        variables,
                        scenarios = NULL,
                        transformation = "reldiff",
                        labels = NULL,
                        startyear = NULL,
                        endyear = NULL,
                        base_year = NULL,
                        name_baseline = "baseline",
                        palette = NULL,
                        colour_by = NULL,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        digits = 2,
                        x_breaks = NULL,
                        interactive = FALSE) {

  if (length(transformation) != 1 || !is.null(names(transformation))) {
    stop("`simple_plot()` draws one y axis, so it takes a single `transformation`. Use `table_3me()` to mix them.")
  }
  spec <- transformation_spec(transformation)

  missing_vars <- setdiff(variables, unique(data$variable))
  if (length(missing_vars) == length(variables)) {
    stop(paste("None of these variables are in the data:", paste(variables, collapse = ", ")))
  }
  if (length(missing_vars) > 0) {
    warning(paste("Variables not found in the data, dropped:", paste(missing_vars, collapse = ", ")))
    variables <- setdiff(variables, missing_vars)
  }

  scenarios_kept <- if (is.null(scenarios)) unique(data$scenario) else scenarios
  if (spec$drop_baseline) scenarios_kept <- union(scenarios_kept, name_baseline)

  if (is.null(startyear)) startyear <- min(data$year, na.rm = TRUE)
  if (is.null(endyear))   endyear   <- max(data$year, na.rm = TRUE)

  kept <- data |>
    dplyr::filter(
      .data$variable %in% variables,
      .data$scenario %in% scenarios_kept
    )

  ## Everything the tooltip may want, computed before the plotted
  ## transformation narrows the data down.
  keys <- series_keys(kept)
  hover <- kept |>
    dplyr::arrange(.data$year) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::mutate(
      .gr = .data$values / dplyr::lag(.data$values) - 1,
      .rel = if ("values_ref" %in% names(kept)) .data$values / .data$values_ref - 1 else NA_real_
    ) |>
    dplyr::ungroup() |>
    ## `values` and `values_ref` are already carried by threeme_transform();
    ## re-joining them here would collide into .x / .y suffixes.
    dplyr::select(dplyr::all_of(c(keys, "year")), ".gr", ".rel")

  plot_data <- kept |>
    threeme_transform(
      transformation = transformation,
      name_baseline = name_baseline,
      base_year = base_year
    ) |>
    dplyr::left_join(hover, by = c(keys, "year")) |>
    dplyr::filter(.data$year >= startyear, .data$year <= endyear)

  ## Display labels for the variables, extended with the sector or commodity.
  label_map <- pretty_labels(variables, labels)
  plot_data$label <- unname(label_map[plot_data$variable])
  plot_data <- with_sector_labels(plot_data)

  if (is.null(colour_by)) {
    colour_by <- if (length(unique(plot_data$label)) == 1) "scenario" else "variable"
  }
  colour_by <- match.arg(colour_by, c("variable", "scenario"))

  colour_col <- if (colour_by == "variable") "label" else "scenario"
  line_col   <- if (colour_by == "variable") "scenario" else "label"

  colour_keys <- unique(plot_data[[colour_col]])
  default_type <- if (colour_by == "variable") "distinct" else "gradient"
  ## Named defaults for the usual macro aggregates, when plotting by variable.
  if (colour_by == "variable" && is.null(palette)) {
    known <- threeme_variable_palette()
    palette <- known[intersect(names(known), colour_keys)]
    if (length(palette) == 0) palette <- NULL
  }
  pal <- threeme_palette(keys = colour_keys, palette = palette, type = default_type)

  n_years <- endyear - startyear
  if (is.null(x_breaks)) {
    x_breaks <- if (n_years <= 10) 1 else if (n_years <= 20) 2 else if (n_years <= 35) 5 else 10
  }
  break_x_sequence <- seq(from = startyear, to = endyear, by = x_breaks)

  y_labels <- if (spec$percent) {
    scales::percent_format(accuracy = 10^(-digits) * 100)
  } else {
    scales::label_number(accuracy = 10^(-digits))
  }

  single_line_dim <- length(unique(plot_data[[line_col]])) == 1

  ## Built before the plot so that the tooltip travels with the plot data.
  if (interactive) {
    fmt_pct <- function(x) {
      ifelse(is.na(x), "n/a", paste0(format(round(x * 100, digits), nsmall = digits), " %"))
    }
    fmt_num <- function(x) {
      ifelse(is.na(x), "n/a", format(round(x, digits), nsmall = digits))
    }
    plot_data$.tooltip <- paste0(
      plot_data$label, " - ", plot_data$scenario, " (", plot_data$year, ")",
      "\nlevel: ", fmt_num(plot_data$values),
      if ("values_ref" %in% names(plot_data)) {
        paste0("\nbaseline: ", fmt_num(plot_data$values_ref))
      } else {
        ""
      },
      "\ngrowth: ", fmt_pct(plot_data$.gr),
      "\nrel. diff: ", fmt_pct(plot_data$.rel)
    )
  }

  p <- ggplot(plot_data, aes(x = .data$year, y = .data$value)) +
    geom_line(aes(
      colour = .data[[colour_col]],
      linetype = .data[[line_col]]
    )) +
    scale_x_continuous(breaks = break_x_sequence) +
    scale_y_continuous(labels = y_labels) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(
      x = NULL, y = NULL,
      title = title, subtitle = subtitle,
      caption = caption %||% spec$label,
      linetype = NULL
    ) +
    ofce::theme_ofce() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks = element_line(linewidth = 0.5, colour = "grey42"),
      legend.position = "bottom"
    )

  ## A single scenario (or a single variable) needs no linetype legend.
  if (single_line_dim) p <- p + scale_linetype(guide = "none")

  if (!interactive) return(p)

  p <- p + ggiraph::geom_point_interactive(
    aes(
      colour = .data[[colour_col]],
      tooltip = .data$.tooltip,
      data_id = paste(.data[[colour_col]], .data$year)
    ),
    size = 0.8
  )

  ofce::girafy(p)
}
