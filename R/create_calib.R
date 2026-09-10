## Creating scenario calibration scripts.
##
## ThreeME reads its scenarios from `configuration/scenarii_calib/`, under a
## naming convention `readconfig()` depends on:
##
##   1_calib_<scenario_baseline>.R   the baseline
##   2_calib_shock_<scenario>.R      each shock
##
## Getting that name wrong produces a config that points at a file which is not
## there, so the name is built here rather than typed.
##
## The folder is a configuration option, one for the baseline and one for the
## shocks (`baseline_scenario_folder`, `shock_scenario_folder`), so a set of
## scenarios can be kept together in a subfolder of `scenarii_calib`.

#' The folders scenario calibration scripts are kept in
#'
#' @description `calib_folder_default()` is where ThreeME looks for them unless
#'   the configuration says otherwise, through `baseline_scenario_folder` and
#'   `shock_scenario_folder`. `list_calib_folders()` lists that folder and its
#'   subfolders, which is what those two options are meant to point at.
#'
#' @param path the calibration folder, relative to `root`.
#' @param root the project root.
#'
#' @returns `calib_folder_default()` returns `"configuration/scenarii_calib"`.
#'   `list_calib_folders()` returns `path` followed by its subfolders, all
#'   relative to `root`, as they go into the configuration.
#' @export
#'
#' @examples
#' calib_folder_default()
#' \dontrun{
#' list_calib_folders()
#' }
calib_folder_default <- function() file.path("configuration", "scenarii_calib")

#' @rdname calib_folder_default
#' @export
list_calib_folders <- function(path = calib_folder_default(), root = ".") {
  full <- in_project(path, root)
  if (!dir.exists(full)) return(path)
  sub <- list.dirs(full, full.names = FALSE, recursive = TRUE)
  sub <- sub[nzchar(sub)]
  c(path, if (length(sub)) file.path(path, sort(sub)))
}

#' A path of the configuration, seen from the working directory
#'
#' @param path a path relative to the project root.
#' @param root the project root.
#'
#' @returns the path to use from the working directory.
#' @keywords internal
in_project <- function(path, root = ".") {
  if (identical(root, ".")) path else file.path(root, path)
}

#' Where a scenario calibration script lives
#'
#' @param name scenario name.
#' @param type `"baseline"` or `"shock"`.
#' @param path the folder the script is in: `scenarii_calib`, or the
#'   subfolder the configuration names in `baseline_scenario_folder` /
#'   `shock_scenario_folder`.
#'
#' @returns the file path, following the convention [readconfig()] expects.
#' @export
#'
#' @examples
#' calib_path("steady", "baseline", path = "configuration/scenarii_calib")
#' calib_path("ct1", "shock", path = "configuration/scenarii_calib")
calib_path <- function(name, type = c("baseline", "shock"),
                       path = calib_folder_default()) {
  type <- match.arg(type)
  file.path(path, paste0(calib_file_name(name, type)))
}

#' @rdname calib_path
#' @returns `calib_file_name()` returns the bare file name.
#' @export
calib_file_name <- function(name, type = c("baseline", "shock")) {
  type <- match.arg(type)
  name <- calib_scenario_name(name, type)
  if (type == "baseline") paste0("1_calib_", name, ".R")
  else paste0("2_calib_shock_", name, ".R")
}

#' The scenario name as it goes into the configuration
#'
#' Validates the name and, for a baseline, makes sure it carries the `baseline`
#' prefix the file convention needs: `readconfig()` builds the path as
#' `1_calib_<scenario_baseline>.R`, so a baseline called `ademe` has to become
#' `baseline_ademe` for the file to be found.
#'
#' @param name scenario name.
#' @param type `"baseline"` or `"shock"`.
#'
#' @returns the normalised name.
#' @export
#'
#' @examples
#' calib_scenario_name("ademe", "baseline")
#' calib_scenario_name("baseline-steady", "baseline")
#' calib_scenario_name("ct1", "shock")
calib_scenario_name <- function(name, type = c("baseline", "shock")) {
  type <- match.arg(type)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    cli::cli_abort("`name` must be a single non-empty string.")
  }
  if (grepl("[A-Z]", name)) {
    cli::cli_abort(c("{.arg name} must be lower case: got {.val {name}}.",
                     "i" = "The configuration lower-cases scenario names, so an upper-case name would not match its own file."))
  }
  if (!grepl("^[a-z][a-z0-9_-]*$", name)) {
    cli::cli_abort("{.arg name} must start with a letter and hold only lower-case letters, digits, {.val _} and {.val -}: got {.val {name}}.")
  }
  if (type == "baseline" && !grepl("^baseline", name)) {
    name <- paste0("baseline_", name)
  }
  name
}

#' Create a scenario calibration script from a template
#'
#' @description Writes the skeleton of a baseline or shock calibration into
#'   `configuration/scenarii_calib/`, named so that [readconfig()] finds it.
#'   The skeleton loads the calibration, selects a year and one inconsequential
#'   variable (world demand), applies a neutral transformation, and ends on the
#'   `baseline_ch` / `shock_ch` object the pipeline reads -- so it runs as
#'   written, and produces no change until you edit it.
#'
#' @param name scenario name: lower case, starting with a letter, and otherwise
#'   letters, digits, `_` and `-`. For a baseline a `baseline_` prefix is added
#'   if it is not there already.
#' @param type `"baseline"` or `"shock"`.
#' @param title a one-line description, written into the header. Defaults to the
#'   name.
#' @param path the folder to write into: `scenarii_calib`, or a subfolder of
#'   it. A subfolder also has to be named in the configuration, as
#'   `baseline_scenario_folder` or `shock_scenario_folder`.
#' @param overwrite `TRUE` replaces an existing file. The default refuses,
#'   because a scenario script is hand-written work.
#' @param open `TRUE` opens the new file in RStudio when RStudio is available.
#' @param quiet `TRUE` suppresses the message.
#'
#' @returns invisibly, the path written.
#' @export
#'
#' @examples
#' \dontrun{
#' create_baseline("ademe")          # -> 1_calib_baseline_ademe.R
#' create_shock("ct2", title = "2 GDP points of carbon tax")
#' }
create_calib <- function(name, type = c("baseline", "shock"), title = NULL,
                         path = calib_folder_default(),
                         overwrite = FALSE, open = TRUE, quiet = FALSE) {
  type <- match.arg(type)
  scenario <- calib_scenario_name(name, type)
  target <- calib_path(name, type, path = path)

  if (!dir.exists(path)) {
    cli::cli_abort(c("no such folder: {.path {path}}",
                     "i" = "Run this from the root of a ThreeME project, or pass {.arg path}."))
  }
  if (file.exists(target) && !overwrite) {
    cli::cli_abort(c("{.file {target}} already exists.",
                     "i" = "Pass {.code overwrite = TRUE} to replace it, or pick another name."))
  }

  template <- system.file("templates", paste0("calib_", type, ".R"),
                          package = "ermeeth2")
  if (!nzchar(template)) cli::cli_abort("the {type} template is missing from the package.")

  body <- readLines(template, warn = FALSE)
  body <- gsub("{{title}}", if (is.null(title)) scenario else title, body, fixed = TRUE)
  body <- gsub("{{date}}", format(Sys.Date()), body, fixed = TRUE)
  writeLines(body, target)

  if (!quiet) {
    setting <- if (type == "baseline") {
      paste0("scenario_baseline = \"", scenario, "\"")
    } else {
      paste0("scenario = c(\"", scenario, "\")")
    }
    folder_setting <- if (basename(path) != basename(calib_folder_default())) {
      paste0(type, "_scenario_folder = \"", path, "\"")
    }
    cli::cli_inform(c("v" = "Created {.file {target}}",
                      "i" = "Point the configuration at it with {.code {setting}}.",
                      if (!is.null(folder_setting)) c("i" = "It is not in the default folder, so the configuration also needs {.code {folder_setting}}.")))
  }
  if (open && rstudioapi::isAvailable()) {
    try(rstudioapi::navigateToFile(target), silent = TRUE)
  }
  invisible(target)
}

#' @rdname create_calib
#' @export
create_baseline <- function(name, title = NULL,
                            path = calib_folder_default(),
                            overwrite = FALSE, open = TRUE, quiet = FALSE) {
  create_calib(name, type = "baseline", title = title, path = path,
               overwrite = overwrite, open = open, quiet = quiet)
}

#' @rdname create_calib
#' @export
create_shock <- function(name, title = NULL,
                         path = calib_folder_default(),
                         overwrite = FALSE, open = TRUE, quiet = FALSE) {
  create_calib(name, type = "shock", title = title, path = path,
               overwrite = overwrite, open = open, quiet = quiet)
}

#' List the scenario calibrations a project holds
#'
#' @param path the folder to look in: `scenarii_calib`, or a subfolder of it.
#'   Subfolders are not searched.
#' @param type which to list.
#'
#' @returns a data frame with `name` (as it goes into the configuration), `type`
#'   and `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_calibs()
#' }
list_calibs <- function(path = calib_folder_default(),
                        type = c("both", "baseline", "shock")) {
  type <- match.arg(type)
  if (!dir.exists(path)) {
    return(data.frame(name = character(0), type = character(0),
                      file = character(0), stringsAsFactors = FALSE))
  }
  files <- list.files(path, pattern = "\\.R$")

  base <- files[grepl("^1_calib_", files)]
  shock <- files[grepl("^2_calib_shock_", files)]
  out <- rbind(
    data.frame(name = sub("^1_calib_(.*)\\.R$", "\\1", base),
               type = rep("baseline", length(base)),
               file = file.path(path, base), stringsAsFactors = FALSE),
    data.frame(name = sub("^2_calib_shock_(.*)\\.R$", "\\1", shock),
               type = rep("shock", length(shock)),
               file = file.path(path, shock), stringsAsFactors = FALSE)
  )
  if (type != "both") out <- out[out$type == type, , drop = FALSE]
  rownames(out) <- NULL
  out
}
