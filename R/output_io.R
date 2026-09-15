#' Write a ThreeME output database
#'
#' @description Writes one of the simulation output databases to `data/output`,
#'   in any combination of RDS and Parquet. Parquet is written with zstd
#'   compression; it is about as small as a compressed `.rds`, roughly twice as
#'   fast to write, and can be read back selectively by [read_3me_output()].
#'
#' @param data data.frame. The database to write.
#' @param project_name character. Project name, used as the file stem.
#' @param suffix character or `NULL`. Aggregation level suffix, e.g. `"com"`,
#'   `"sec"`, `"sec_com"`. `NULL` for the main database.
#' @param formats character vector. `"rds"`, `"parquet"` or both.
#' @param dir character. Output directory.
#'
#' @returns The paths written, invisibly.
#' @export
save_3me_output <- function(data,
                            project_name,
                            suffix = NULL,
                            formats = c("rds", "parquet"),
                            dir = file.path("data", "output")) {

  formats <- match.arg(formats, c("rds", "parquet"), several.ok = TRUE)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  stem <- if (is.null(suffix) || !nzchar(suffix)) project_name else paste0(project_name, "_", suffix)
  written <- character(0)

  if ("rds" %in% formats) {
    f <- file.path(dir, paste0(stem, ".rds"))
    saveRDS(data, file = f)
    written <- c(written, f)
  }

  if ("parquet" %in% formats) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      message_warning("Package 'arrow' is not installed, the parquet output was skipped. Install it with pak::pak('arrow').")
    } else {
      f <- file.path(dir, paste0(stem, ".parquet"))
      arrow::write_parquet(data, f, compression = "zstd")
      written <- c(written, f)
    }
  }

  invisible(written)
}


#' Read a ThreeME output database
#'
#' @description Reads back a database written by [save_3me_output()]. When a
#'   parquet file is available, `variables`, `scenarios` and `years` are pushed
#'   down into the read, so only the rows and columns asked for are ever
#'   materialised; filtering a whole `.rds` afterwards costs the full load.
#'
#' @param project_name character. Project name.
#' @param suffix character or `NULL`. `NULL` (default) for the main database,
#'   otherwise `"com"`, `"sec"` or `"sec_com"`.
#' @param variables character vector or `NULL`. Keep only these variables.
#' @param scenarios character vector or `NULL`. Keep only these scenarios.
#' @param years numeric vector or `NULL`. Keep only these years.
#' @param columns character vector or `NULL`. Keep only these columns.
#' @param dir character. Directory holding the output files.
#'
#' @returns A data.frame.
#' @export
read_3me_output <- function(project_name,
                            suffix = NULL,
                            variables = NULL,
                            scenarios = NULL,
                            years = NULL,
                            columns = NULL,
                            dir = file.path("data", "output")) {

  variable <- NULL
  scenario <- NULL
  year <- NULL

  stem <- if (is.null(suffix) || !nzchar(suffix)) project_name else paste0(project_name, "_", suffix)
  pq  <- file.path(dir, paste0(stem, ".parquet"))
  rds <- file.path(dir, paste0(stem, ".rds"))

  use_parquet <- file.exists(pq) && requireNamespace("arrow", quietly = TRUE)

  if (use_parquet) {
    out <- arrow::open_dataset(pq)
    if (!is.null(variables)) out <- dplyr::filter(out, variable %in% variables)
    if (!is.null(scenarios)) out <- dplyr::filter(out, scenario %in% scenarios)
    if (!is.null(years))     out <- dplyr::filter(out, year %in% years)
    if (!is.null(columns))   out <- dplyr::select(out, dplyr::all_of(columns))
    out <- dplyr::collect(out)
    ## a partitioned dataset returns its partition key as a factor
    if ("scenario" %in% names(out) && is.factor(out$scenario)) {
      out$scenario <- as.character(out$scenario)
    }
    return(as.data.frame(out))
  }

  if (!file.exists(rds)) {
    stop("No output found for '", stem, "' in ", dir, " (looked for .parquet and .rds).")
  }

  out <- readRDS(rds)
  if (!is.null(variables)) out <- out[out$variable %in% variables, , drop = FALSE]
  if (!is.null(scenarios) && "scenario" %in% names(out)) out <- out[out$scenario %in% scenarios, , drop = FALSE]
  if (!is.null(years))     out <- out[out$year %in% years, , drop = FALSE]
  if (!is.null(columns))   out <- out[, columns, drop = FALSE]
  rownames(out) <- NULL
  as.data.frame(out)
}
