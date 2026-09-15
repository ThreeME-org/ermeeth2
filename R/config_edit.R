## Reading and editing ThreeME configuration files.
##
## A configuration file is R code, not data: `calib_files` carries a long vector
## threaded with `# ALL VERSIONS` comments and commented-out alternatives, and
## the solver section holds a live `if (use.superlu) Sys.setenv(...)` block.
## Regenerating such a file from a template would silently throw all of that
## away, so edits here replace one assignment at a time and leave every other
## byte alone.

#' Locate the top-level assignments in an R script
#'
#' @param lines the file, as a character vector of lines.
#'
#' @returns a data frame with `name`, `first` and `last` (line numbers). Where a
#'   name is assigned more than once the last assignment wins, since that is the
#'   one that takes effect.
#' @keywords internal
config_assignments <- function(lines) {
  exprs <- parse(text = lines, keep.source = TRUE)
  refs <- utils::getSrcref(exprs)
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (!is.call(e) || length(e) < 3L) next
    op <- as.character(e[[1]])
    if (!op %in% c("<-", "=", "<<-")) next
    target <- e[[2]]
    if (!is.name(target)) next
    r <- refs[[i]]
    out[[length(out) + 1L]] <- data.frame(
      name  = as.character(target),
      first = r[1L],
      last  = r[3L],
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) {
    return(data.frame(name = character(0), first = integer(0),
                      last = integer(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, out)
  out[!duplicated(out$name, fromLast = TRUE), , drop = FALSE]
}

#' Replace the value of one assignment, leaving the rest of the file alone
#'
#' @param lines the file, as a character vector of lines.
#' @param name the variable assigned.
#' @param value_code the replacement, as R source text. May span lines.
#' @param add `TRUE` appends the assignment when the file has none. `FALSE`
#'   returns the lines unchanged.
#'
#' @returns the edited lines.
#' @keywords internal
config_set <- function(lines, name, value_code, add = TRUE) {
  asg <- config_assignments(lines)
  new <- paste0(name, " = ", value_code)
  new <- strsplit(paste(new, collapse = "\n"), "\n", fixed = TRUE)[[1]]

  hit <- asg[asg$name == name, , drop = FALSE]
  if (!nrow(hit)) {
    if (!add) return(lines)
    return(c(lines, "", new))
  }
  before <- if (hit$first > 1L) lines[seq_len(hit$first - 1L)] else character(0)
  after  <- if (hit$last < length(lines)) lines[(hit$last + 1L):length(lines)] else character(0)
  c(before, new, after)
}

#' Apply several edits to a configuration file
#'
#' @param file path to the configuration file.
#' @param values a named list of replacements. Each element is either R source
#'   text (wrap it in [I()] to say "this is already code") or a value to be
#'   deparsed.
#' @param out where to write. Defaults to `file`, editing in place.
#'
#' @returns invisibly, the path written.
#' @export
#'
#' @examples
#' \dontrun{
#' config_edit("configuration/config_input_threeme.R",
#'             list(project_name = "my_run", shockyear = 2025))
#' }
config_edit <- function(file, values, out = file) {
  if (!file.exists(file)) stop("no such file: ", file)
  lines <- readLines(file, warn = FALSE)
  for (nm in names(values)) {
    v <- values[[nm]]
    code <- if (inherits(v, "AsIs")) as.character(v) else paste(deparse(v), collapse = "\n")
    lines <- config_set(lines, nm, code)
  }
  ## a last parse check, so a bad edit fails here rather than at run time
  ok <- tryCatch({ parse(text = lines); TRUE }, error = function(e) conditionMessage(e))
  if (!isTRUE(ok)) stop("the edit would produce a file that does not parse: ", ok)
  writeLines(lines, out)
  invisible(out)
}

#' The configuration fields this package knows how to edit
#'
#' Held as data so the addin builds its controls from it rather than repeating
#' the list. Anything not named here is carried through an edit untouched.
#'
#' @returns a data frame with `name`, `section`, `type` and `label`.
#' @export
#'
#' @examples
#' config_fields()
config_fields <- function() {
  f <- function(name, section, type, label) {
    data.frame(name = name, section = section, type = type, label = label,
               stringsAsFactors = FALSE)
  }
  rbind(
    f("project_name",     "Basics", "text",   "Project name"),
    f("iso3",             "Basics", "text",   "Country (ISO3)"),
    f("classification",   "Basics", "text",   "Classification"),
    f("model_folder",     "Basics", "text",   "Model folder"),
    f("baseyear",         "Basics", "number", "Base year"),
    f("lastyear",         "Basics", "number", "Last year"),
    f("shockyear",        "Basics", "number", "Shock year"),
    f("max_lags",         "Basics", "number", "Maximum lag"),
    f("automated_shocks", "Basics", "bool",   "Automated shocks"),

    f("Rsolver",               "Solver", "bool",   "Use the R solver (else EViews)"),
    f("warning",               "Solver", "bool",   "Solver warning messages"),
    f("tolerance_calib_check", "Solver", "number", "Calibration check tolerance"),
    f("skip_compiler",         "Solver", "bool",   "Skip the compiler"),
    f("recompile_model",       "Solver", "bool",   "Recompile the model"),
    f("save_files_res",        "Solver", "bool",   "Save all result files"),
    f("rcpp_option",           "Solver", "bool",   "Use Rcpp"),
    f("use.superlu",           "Solver", "bool",   "Use SuperLU"),
    f("eviews_timeout",        "Solver", "number", "EViews timeout (s, 0 = none)"),
    f("path_eviews_exe",       "Solver", "text",   "Path to EViews.exe")
  )
}

#' Read the values a configuration file assigns
#'
#' Sources the file in a throwaway environment, so the values come back exactly
#' as the pipeline would see them -- `firstyear` computed from `baseyear`,
#' `calib_files` built from `iso3`, and so on.
#'
#' @param file path to a configuration input file.
#' @param fields which names to return. Defaults to everything the file assigns.
#' @param with a named list injected before sourcing. An **output**
#'   configuration is not self-contained -- `quartos_parameters` refers to
#'   `project_name`, `lastyear` and `scenario`, which the *input* configuration
#'   defines -- so reading one on its own fails unless the input values are
#'   passed in here.
#'
#' @returns a named list.
#' @export
#'
#' @examples
#' \dontrun{
#' vals <- read_config_values("configuration/config_input_threeme.R")
#' read_config_values("configuration/config_output_threeme.R", with = vals)
#' }
read_config_values <- function(file, fields = NULL, with = list()) {
  if (!file.exists(file)) stop("no such file: ", file)
  env <- new.env(parent = globalenv())
  ## The configs call set_names() and str_c() without qualifying them.
  env$set_names <- rlang::set_names
  env$str_c <- stringr::str_c
  injected <- names(with)
  for (nm in injected) assign(nm, with[[nm]], envir = env)
  sys.source(file, envir = env, keep.source = FALSE)
  nms <- if (is.null(fields)) {
    setdiff(ls(env), c("set_names", "str_c", injected))
  } else fields
  out <- lapply(nms, function(n) if (exists(n, envir = env, inherits = FALSE)) get(n, envir = env) else NULL)
  stats::setNames(out, nms)
}

#' Find the configuration files of a project
#'
#' @param path the configuration folder.
#'
#' @returns a data frame with `name`, `kind` (`input` or `output`) and `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_configs()
#' }
list_configs <- function(path = "configuration") {
  if (!dir.exists(path)) {
    return(data.frame(name = character(0), kind = character(0),
                      file = character(0), stringsAsFactors = FALSE))
  }
  files <- list.files(path, pattern = "^config_(input|output)_.*\\.R$")
  if (!length(files)) {
    return(data.frame(name = character(0), kind = character(0),
                      file = character(0), stringsAsFactors = FALSE))
  }
  kind <- ifelse(grepl("^config_input_", files), "input", "output")
  name <- sub("^config_(input|output)_(.*)\\.R$", "\\2", files)
  data.frame(name = name, kind = kind, file = file.path(path, files),
             stringsAsFactors = FALSE)
}

#' Is this a ThreeME project?
#'
#' The addins edit files at fixed paths inside a ThreeME v4 project --
#' `configuration/`, `configuration/scenarii_calib/`, `src/model/` -- so opening
#' one from an unrelated working directory can only misfire. This is the check
#' they run before doing anything.
#'
#' @param path the project root.
#'
#' @returns a list with `ok` (logical) and `missing` (the expected folders that
#'   are not there).
#' @export
#'
#' @examples
#' is_threeme_project(tempdir())
is_threeme_project <- function(path = ".") {
  expected <- c("configuration",
                file.path("configuration", "scenarii_calib"),
                "src")
  missing <- expected[!dir.exists(file.path(path, expected))]
  list(ok = length(missing) == 0L, missing = missing)
}

#' Stop unless the working directory is a ThreeME project
#'
#' @param path the project root.
#' @param what the addin name, for the message.
#'
#' @returns invisibly `TRUE`, or an error.
#' @keywords internal
check_threeme_project <- function(path = ".", what = "This addin") {
  chk <- is_threeme_project(path)
  if (chk$ok) return(invisible(TRUE))
  stop(what, " only works inside a ThreeME v4 project.\n",
       "Missing from ", normalizePath(path, mustWork = FALSE), ": ",
       paste(chk$missing, collapse = ", "), ".\n",
       "Open the ThreeME project (its .Rproj) and try again.",
       call. = FALSE)
}

#' A warning banner for an addin opened outside a ThreeME project
#'
#' @param path the project root.
#'
#' @returns a shiny tag, or `NULL` when the project looks right.
#' @keywords internal
project_banner <- function(path = ".") {
  chk <- is_threeme_project(path)
  if (chk$ok) return(NULL)
  shiny::div(
    class = "alert alert-danger",
    shiny::tags$b("This is not a ThreeME v4 project."),
    shiny::br(),
    "These addins read and write files at fixed paths inside a ThreeME ",
    "project. Missing here: ",
    shiny::tags$code(paste(chk$missing, collapse = ", ")), ".",
    shiny::br(),
    shiny::tags$small("Open the ThreeME project (its .Rproj) and launch the addin again.")
  )
}
