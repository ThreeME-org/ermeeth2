## Parametric shocks: one shock sheet run for several values of its parameters.
##
##   2_calib_shock_<shock>.R             the shock sheet, an ordinary shock
##                                       script that declares its parameters
##                                       with shock_parameters()
##   3_params_<shock>_<identifier>.R     a parameter sheet: the values to test,
##                                       as a shock_grid()
##
## The configuration names the parameter sheets to run, never the values:
##
##   parametric_shocks = list(ct = c("redis", "traj"))
##
## Each row of a grid is one scenario, `<shock>_<identifier>_<variation>`
## (`ct_redis_1`, `ct_redis_2`, ...). The values each scenario ran with are
## written next to the output databases, in `<project_name>_params.csv`.

## Columns of the metadata file, which a parameter cannot be called.
params_reserved <- function() c("scenario", "shock", "identifier", "variation")

#' Declare the parameters of a shock sheet
#'
#' @description Placed at the top of a shock calibration script
#'   (`2_calib_shock_<name>.R`), `shock_parameters()` declares the values the
#'   script can be run with, and creates each of them as a variable the rest of
#'   the script uses like any other.
#'
#'   * Run as an ordinary scenario (listed in `scenario` in the configuration),
#'     the script gets the defaults written here.
#'   * Run as a parametric shock (listed in `parametric_shocks`), it gets the
#'     values of one row of a parameter sheet ([shock_grid()]) instead. A
#'     parameter the grid does not vary keeps its default.
#'
#'   The script is otherwise an ordinary shock script, ending on `shock_ch`.
#'
#'   Tested line by line in the calibration bubble ([calibration_environment()]),
#'   the script gets the defaults, unless a parametric scenario was chosen with
#'   `calibration_environment(scenario = )` or [use_parametric_scenario()]:
#'   it then gets the values of that scenario, and says so.
#'
#' @param ... the parameters and their defaults, as `name = value`.
#'
#' @returns invisibly, the named list of the values in use. Called for its
#'   side effect: each parameter is assigned in the calling environment.
#' @export
#' @seealso [shock_grid()] for the parameter sheets, [create_params()] to
#'   write one.
#'
#' @examples
#' shock_parameters(ct_start = 100, ct_end = 200)
#' ct_end
shock_parameters <- function(...) {
  defaults <- list(...)
  check_param_names(names(defaults), "shock_parameters()")
  env <- parent.frame()
  values <- defaults
  ## The values of a parametric run, put there by run_simulations(), or by
  ## the calibration bubble for a scenario chosen to test the sheet with.
  given <- get0(".shock_parameters", envir = env, inherits = FALSE)
  if (!is.null(given)) {
    unknown <- setdiff(names(given), names(defaults))
    if (length(unknown)) {
      cli::cli_abort(c("The parameter sheet sets {.code {unknown}}, which this shock sheet does not declare.",
                       "i" = "It declares {.code {names(defaults)}}."))
    }
    values[names(given)] <- given
    assign(".shock_parameters_used", values, envir = env)
    ## Tested by hand: say that these are not the defaults.
    if (!is.null(attr(given, "scenario"))) parametric_values_message(given, defaults = setdiff(names(defaults), names(given)))
  }
  for (nm in names(values)) assign(nm, values[[nm]], envir = env)
  invisible(values)
}

#' Test a shock sheet with the values of a parametric scenario
#'
#' @description In the calibration bubble ([calibration_environment()]), a
#'   shock sheet run line by line gets its defaults from [shock_parameters()].
#'   `use_parametric_scenario("ct_redis_4")` makes it get the values of that
#'   scenario instead, until `use_parametric_scenario(NULL)` brings back the
#'   defaults. The bubble holds `parametric_grid`, the list of the parametric
#'   scenarios and their values.
#'
#'   The choice is kept in `.shock_parameters`, a hidden object of `envir`,
#'   which `rm(list = names(calibration_bubble))` removes with the rest of the
#'   bubble.
#'
#' @param scenario a parametric scenario of the configuration, or `NULL` for
#'   the defaults.
#' @param configuration the configuration, from [readconfig()].
#' @param envir where the shock sheet is run, the global environment when it
#'   is run line by line.
#'
#' @returns invisibly, the values of the scenario.
#' @export
#'
#' @examples
#' \dontrun{
#' calibration_bubble <- calibration_environment(FALSE, config)
#' list2env(calibration_bubble, envir = globalenv())
#' parametric_grid
#' use_parametric_scenario("ct_redis_4")
#' ## ... run 2_calib_shock_ct.R line by line, look at shock_ch ...
#' use_parametric_scenario(NULL)
#' }
use_parametric_scenario <- function(scenario, configuration = config, envir = globalenv()) {
  if (is.null(scenario)) {
    if (exists(".shock_parameters", envir = envir, inherits = FALSE)) {
      rm(".shock_parameters", envir = envir)
    }
    cli::cli_alert_info("{.fn shock_parameters} now gives the defaults of the shock sheet.")
    return(invisible(NULL))
  }
  values <- parametric_values(scenario, configuration$input$parametric_runs)
  assign(".shock_parameters", values, envir = envir)
  parametric_values_message(values)
  invisible(values)
}

#' The values of one parametric scenario
#'
#' @param scenario the scenario.
#' @param runs the parametric scenarios, from [parametric_runs()].
#'
#' @returns the named list of the values its grid sets, with the scenario as
#'   attribute `scenario`.
#' @keywords internal
parametric_values <- function(scenario, runs) {
  if (!is.character(scenario) || length(scenario) != 1L || !scenario %in% names(runs)) {
    if (!length(runs)) {
      cli::cli_abort("The configuration has no parametric scenario: set {.code parametric_shocks}.", call = NULL)
    }
    cli::cli_abort(c("{.val {scenario}} is not a parametric scenario of the configuration.",
                     "i" = "They are {.val {names(runs)}}."), call = NULL)
  }
  structure(runs[[scenario]]$values, scenario = scenario)
}

parametric_values_message <- function(values, defaults = NULL) {
  shown <- paste0(names(values), " = ", vapply(values, format, character(1)), collapse = ", ")
  cli::cli_alert_info(paste0(
    "{.fn shock_parameters} gives the values of {.val {attr(values, 'scenario')}}: {shown}",
    if (length(defaults)) "; {.code {defaults}} at {?its/their} default{?s}",
    "."))
}

#' The values a parametric shock is run with
#'
#' @description The body of a parameter sheet
#'   (`3_params_<shock>_<identifier>.R`). Each argument is a parameter of the
#'   shock sheet and the values to test for it; every combination is run, as
#'   one scenario.
#'
#'   The result is a plain data frame, one row per scenario, so it can be cut
#'   down before it is returned, e.g. with `dplyr::filter()`. Whatever the
#'   sheet evaluates to last is the grid that runs; it may also be a data frame
#'   built by other means, one column per parameter.
#'
#' @param ... the parameters and their values, as `name = c(...)`. Numbers,
#'   strings or logicals, one value per scenario: a trajectory is two
#'   parameters (a start and an end value), not a vector.
#'
#' @returns a data frame, one column per parameter and one row per
#'   combination. The first parameter varies slowest, the last one fastest, so
#'   the rows come in the order nested loops would give.
#' @export
#' @seealso [shock_parameters()], [create_params()].
#'
#' @examples
#' shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 0.5, 1))
#'
#' ## a grid can be cut down: here, never more than full redistribution in all
#' shock_grid(redis_h = c(0, 0.5, 1), redis_ls = c(0, 0.5, 1)) |>
#'   subset(redis_h + redis_ls <= 1)
shock_grid <- function(...) {
  values <- list(...)
  if (!length(values)) cli::cli_abort("{.fn shock_grid} needs at least one parameter.")
  check_param_names(names(values), "shock_grid()")
  for (nm in names(values)) check_param_values(values[[nm]], nm)
  values <- lapply(values, unique)
  grid <- expand.grid(rev(values), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[names(values)]
  rownames(grid) <- NULL
  grid
}

check_param_names <- function(nms, where) {
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("Every argument of {.fn {where}} must be named: {.code name = value}.", call = NULL)
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup)) cli::cli_abort("{.fn {where}} names {.code {dup}} more than once.", call = NULL)
  bad <- nms[make.names(nms) != nms]
  if (length(bad)) {
    cli::cli_abort("{.code {bad}} {?is not a/are not} valid R name{?s}: a parameter becomes a variable of the shock sheet.", call = NULL)
  }
  res <- intersect(nms, params_reserved())
  if (length(res)) {
    cli::cli_abort("{.code {res}} cannot be a parameter name: {.code {params_reserved()}} are the columns of the metadata file.", call = NULL)
  }
}

check_param_values <- function(x, nm) {
  if (is.factor(x)) x <- as.character(x)
  if (!(is.numeric(x) || is.character(x) || is.logical(x)) || !is.null(dim(x)) || !length(x)) {
    cli::cli_abort("The values of {.code {nm}} must be a vector of numbers, strings or logicals.", call = NULL)
  }
  if (anyNA(x)) cli::cli_abort("The values of {.code {nm}} hold an {.code NA}.", call = NULL)
  invisible(x)
}

#' Where a parameter sheet lives
#'
#' @param shock the shock sheet the parameters are for: `ct` for
#'   `2_calib_shock_ct.R`.
#' @param identifier what tells the parameter sheets of one shock apart:
#'   `redis` for `3_params_ct_redis.R`. Lower case letters, digits, `_` and `-`.
#' @param path the folder of the parameter sheets (`shock_params_folder` in
#'   the configuration).
#'
#' @returns the file path.
#' @export
#'
#' @examples
#' params_path("ct", "redis", path = "configuration/scenarii_calib")
params_path <- function(shock, identifier, path = calib_folder_default()) {
  file.path(path, params_file_name(shock, identifier))
}

#' @rdname params_path
#' @returns `params_file_name()` returns the bare file name.
#' @export
params_file_name <- function(shock, identifier) {
  shock <- calib_scenario_name(shock, "shock")
  check_params_identifier(identifier)
  paste0("3_params_", shock, "_", identifier, ".R")
}

check_params_identifier <- function(identifier) {
  if (!is.character(identifier) || length(identifier) != 1L || is.na(identifier) ||
      !grepl("^[a-z0-9][a-z0-9_-]*$", identifier)) {
    cli::cli_abort("A parameter sheet identifier is lower case letters, digits, {.val _} and {.val -}: got {.val {identifier}}.", call = NULL)
  }
  invisible(identifier)
}

#' List the parameter sheets of a folder
#'
#' @description The shock a sheet belongs to cannot be read off its name alone
#'   when the shock name holds a `_` (`3_params_ct_x_redis.R` is shock `ct_x`
#'   or shock `ct`). Given the shocks that exist, the longest one that fits is
#'   taken; without them, the name is split at its first `_`.
#'
#' @param path the folder of the parameter sheets.
#' @param shocks the names of the shock sheets, to tell the shock from the
#'   identifier.
#'
#' @returns a data frame with `shock`, `identifier` and `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_params(shocks = list_calibs(type = "shock")$name)
#' }
list_params <- function(path = calib_folder_default(), shocks = NULL) {
  none <- data.frame(shock = character(0), identifier = character(0),
                     file = character(0), stringsAsFactors = FALSE)
  if (!dir.exists(path)) return(none)
  files <- list.files(path, pattern = "^3_params_.+\\.R$")
  if (!length(files)) return(none)
  stem <- sub("^3_params_(.*)\\.R$", "\\1", files)
  shocks <- shocks[order(-nchar(shocks))]
  split_one <- function(s) {
    for (sh in shocks) {
      if (startsWith(s, paste0(sh, "_")) && nchar(s) > nchar(sh) + 1L) {
        return(c(sh, substring(s, nchar(sh) + 2L)))
      }
    }
    at <- regexpr("_", s, fixed = TRUE)
    if (at < 2L) return(c(NA_character_, NA_character_))
    c(substr(s, 1L, at - 1L), substring(s, at + 1L))
  }
  parts <- vapply(stem, split_one, character(2), USE.NAMES = FALSE)
  out <- data.frame(shock = parts[1, ], identifier = parts[2, ],
                    file = file.path(path, files), stringsAsFactors = FALSE)
  out <- out[!is.na(out$shock) & nzchar(out$identifier), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Create a parameter sheet
#'
#' @description Writes `3_params_<shock>_<identifier>.R`, the values a
#'   parametric shock is run with. When the shock sheet declares its
#'   parameters ([shock_parameters()]), the new sheet lists each of them at its
#'   default, so it runs as written (one scenario) until values are added.
#'
#' @param shock the shock sheet the parameters are for.
#' @param identifier what tells this parameter sheet apart from the other ones
#'   of the same shock.
#' @param path the folder to write into (`shock_params_folder` in the
#'   configuration).
#' @param shock_folder the folder of the shock sheet
#'   (`shock_scenario_folder`), read for its parameters.
#' @param overwrite `TRUE` replaces an existing file.
#' @param open `TRUE` opens the new file in RStudio when RStudio is available.
#' @param quiet `TRUE` suppresses the message.
#'
#' @returns invisibly, the path written.
#' @export
#'
#' @examples
#' \dontrun{
#' create_params("ct", "redis")    # -> 3_params_ct_redis.R
#' }
create_params <- function(shock, identifier, path = calib_folder_default(),
                          shock_folder = path, overwrite = FALSE,
                          open = TRUE, quiet = FALSE) {
  target <- params_path(shock, identifier, path = path)
  if (!dir.exists(path)) {
    cli::cli_abort(c("no such folder: {.path {path}}",
                     "i" = "Run this from the root of a ThreeME project, or pass {.arg path}."))
  }
  if (file.exists(target) && !overwrite) {
    cli::cli_abort(c("{.file {target}} already exists.",
                     "i" = "Pass {.code overwrite = TRUE} to replace it, or pick another identifier."))
  }
  writeLines(params_template_text(shock, identifier, shock_folder), target)
  if (!quiet) {
    cli::cli_inform(c("v" = "Created {.file {target}}",
                      "i" = "Run it with {.code parametric_shocks = list({shock} = \"{identifier}\")} in the configuration."))
  }
  if (open && rstudioapi::isAvailable()) {
    try(rstudioapi::navigateToFile(target), silent = TRUE)
  }
  invisible(target)
}

#' The text of a new parameter sheet
#'
#' @param shock,identifier the sheet.
#' @param shock_folder where the shock sheet is, read for its parameters.
#'
#' @returns the lines.
#' @keywords internal
params_template_text <- function(shock, identifier, shock_folder) {
  f <- system.file("templates", "calib_params.R", package = "ermeeth2")
  if (!nzchar(f)) cli::cli_abort("the parameter sheet template is missing from the package.")
  body <- readLines(f, warn = FALSE)
  sheet <- calib_path(shock, "shock", path = shock_folder)
  declared <- if (file.exists(sheet)) shock_sheet_parameters(sheet, defaults = TRUE)
  grid <- if (length(declared)) {
    args <- vapply(names(declared), function(nm) {
      paste0("  ", nm, " = c(", declared[[nm]], ")")
    }, character(1))
    c("shock_grid(", paste0(args, c(rep(",", length(args) - 1L), "")), ")")
  } else {
    c("## The shock sheet does not declare its parameters yet: add",
      "## shock_parameters(name = default, ...) at its top, then list the",
      "## values to test here.",
      "shock_grid(", "  parameter = c(1, 2)", ")")
  }
  body <- gsub("{{shock}}", shock, body, fixed = TRUE)
  body <- gsub("{{identifier}}", identifier, body, fixed = TRUE)
  body <- gsub("{{date}}", format(Sys.Date()), body, fixed = TRUE)
  at <- which(body == "{{grid}}")
  c(body[seq_len(at - 1L)], grid, body[-seq_len(at)])
}

#' The parameters a shock sheet declares
#'
#' Reads the [shock_parameters()] call of a shock sheet without running the
#' sheet, which needs the calibration data.
#'
#' @param file the shock sheet.
#' @param defaults `TRUE` to return the default of each parameter, as source
#'   text, rather than only the names.
#'
#' @returns the names of the parameters (or a named character vector of their
#'   defaults), or `NULL` when the sheet has no `shock_parameters()` call.
#' @keywords internal
shock_sheet_parameters <- function(file, defaults = FALSE) {
  exprs <- tryCatch(parse(file, keep.source = FALSE), error = function(e) {
    cli::cli_abort(c("{.file {file}} does not parse.", "x" = conditionMessage(e)), call = NULL)
  })
  is_decl <- function(e) {
    if (!is.call(e)) return(FALSE)
    f <- e[[1]]
    identical(f, as.name("shock_parameters")) ||
      (is.call(f) && identical(f[[1]], as.name("::")) &&
         identical(f[[3]], as.name("shock_parameters")))
  }
  hit <- Filter(is_decl, as.list(exprs))
  if (!length(hit)) return(NULL)
  args <- as.list(hit[[1]])[-1]
  if (!defaults) return(names(args))
  vapply(args, function(a) paste(deparse(a), collapse = " "), character(1))
}

#' Put the parametric shocks of a configuration in one form
#'
#' @param x the `parametric_shocks` entry: a named list of identifiers, or a
#'   named character vector.
#'
#' @returns a named list, one character vector of identifiers per shock.
#' @keywords internal
normalise_parametric_shocks <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  nms <- names(x)
  if (!(is.list(x) || is.character(x)) || is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(c("{.code parametric_shocks} must name the shock of each parameter sheet.",
                     "i" = "For instance {.code parametric_shocks = list(ct = c(\"redis\", \"traj\"))}."),
                   call = NULL)
  }
  ids <- lapply(x, as.character)
  out <- split(unlist(ids, use.names = FALSE),
               factor(rep(nms, lengths(ids)), levels = unique(nms)))
  lapply(out, unique)
}

#' Read a parameter sheet
#'
#' @param file the sheet.
#' @param env the environment it is run in, holding the configuration values.
#'
#' @returns the grid, a data frame of one column per parameter.
#' @keywords internal
read_params_sheet <- function(file, env) {
  grid <- tryCatch(source(file, local = new.env(parent = env))$value, error = function(e) {
    cli::cli_abort(c("Could not run the parameter sheet {.file {file}}.", "x" = conditionMessage(e)),
                   call = NULL)
  })
  if (!is.data.frame(grid) || !nrow(grid) || !ncol(grid)) {
    cli::cli_abort(c("The parameter sheet {.file {file}} must end on a grid of values.",
                     "i" = "Its last line is a {.fn shock_grid}, or a data frame with one column per parameter and one row per scenario."),
                   call = NULL)
  }
  grid <- as.data.frame(grid, stringsAsFactors = FALSE)
  rownames(grid) <- NULL
  tryCatch({
    check_param_names(names(grid), basename(file))
    for (nm in names(grid)) {
      if (is.factor(grid[[nm]])) grid[[nm]] <- as.character(grid[[nm]])
      check_param_values(grid[[nm]], nm)
    }
  }, error = function(e) {
    cli::cli_abort(c("The grid of {.file {file}} is not valid.", "x" = conditionMessage(e)), call = NULL)
  })
  if (anyDuplicated(grid)) {
    cli::cli_abort("The grid of {.file {file}} holds the same values twice: two scenarios would be identical.",
                   call = NULL)
  }
  grid
}

#' Expand the parametric shocks into scenarios
#'
#' @description Reads every parameter sheet named in `parametric_shocks` and
#'   turns each row of its grid into one scenario,
#'   `<shock>_<identifier>_<variation>`. Stops when a sheet is missing, when
#'   the shock sheet does not declare its parameters, or when a grid sets a
#'   parameter the shock sheet does not declare.
#'
#' @param parametric_shocks the `parametric_shocks` entry of the configuration.
#' @param shock_folder,params_folder the folders of the shock sheets and of the
#'   parameter sheets, from the project root.
#' @param env the environment the parameter sheets run in.
#' @param root the project root, seen from the working directory.
#'
#' @returns a list named by scenario. Each element holds `shock`,
#'   `identifier`, `variation`, `file` (the shock sheet) and `values` (the
#'   named list of the parameters the grid sets).
#' @keywords internal
parametric_runs <- function(parametric_shocks, shock_folder, params_folder,
                            env = new.env(), root = ".") {
  ps <- normalise_parametric_shocks(parametric_shocks)
  runs <- list()
  for (shock in names(ps)) {
    sheet <- tryCatch(calib_path(shock, "shock", path = in_project(shock_folder, root)),
                      error = function(e) cli::cli_abort(c("In {.code parametric_shocks}:", "x" = conditionMessage(e)), call = NULL))
    if (!file.exists(sheet)) {
      cli::cli_abort("{.code parametric_shocks} names the shock {.val {shock}}, but {.file {sheet}} does not exist.",
                     call = NULL)
    }
    declared <- shock_sheet_parameters(sheet)
    if (is.null(declared)) {
      cli::cli_abort(c("{.file {sheet}} is used as a parametric shock but does not declare its parameters.",
                       "i" = "Add {.code shock_parameters(name = default, ...)} at its top."),
                     call = NULL)
    }
    for (id in ps[[shock]]) {
      file <- tryCatch(params_path(shock, id, path = in_project(params_folder, root)),
                       error = function(e) cli::cli_abort(c("In {.code parametric_shocks}:", "x" = conditionMessage(e)), call = NULL))
      if (!file.exists(file)) {
        cli::cli_abort(c("No parameter sheet {.file {file}}.",
                         "i" = "Create it with {.code create_params(\"{shock}\", \"{id}\")}, or check {.code shock_params_folder}."),
                       call = NULL)
      }
      grid <- read_params_sheet(file, env)
      unknown <- setdiff(names(grid), declared)
      if (length(unknown)) {
        cli::cli_abort(c("{.file {basename(file)}} sets {.code {unknown}}, which {.file {basename(sheet)}} does not declare.",
                         "i" = "It declares {.code {declared}}."),
                       call = NULL)
      }
      for (i in seq_len(nrow(grid))) {
        runs[[paste(shock, id, i, sep = "_")]] <- list(
          shock = shock, identifier = id, variation = i, file = sheet,
          values = as.list(grid[i, , drop = FALSE]))
      }
    }
  }
  runs
}

#' Set the scenarios of a configuration
#'
#' @description Fills in the scenario entries of `config$input` from the
#'   classic scenarios and the parametric ones, so that every part of the
#'   pipeline sees one list: `scenario`, `scenario_name`, `shocks_nb`,
#'   `calib_scenario` (the script of each scenario: its own for a classic one,
#'   the shock sheet for a parametric one) and `parametric_runs`.
#'
#' @param input `config$input`.
#' @param classic the classic scenarios, possibly named by their display name.
#' @param runs the parametric scenarios, from [parametric_runs()].
#' @param root the project root, seen from the working directory.
#'
#' @returns `input`, updated.
#' @keywords internal
config_scenarios <- function(input, classic, runs = list(), root = ".") {
  classic <- classic %||% character(0)
  labels <- names(classic)
  classic <- tolower(unname(as.character(classic)))
  par_names <- names(runs) %||% character(0)
  scenario <- c(classic, par_names)
  if (!is.null(labels)) {
    labels <- ifelse(is.na(labels) | !nzchar(labels), classic, labels)
    names(scenario) <- c(labels, par_names)
  }
  dup <- unique(scenario[duplicated(scenario)])
  if (length(dup)) {
    cli::cli_abort(c("Two scenarios have the same name: {.val {dup}}.",
                     "i" = "A parametric scenario is called {.code <shock>_<identifier>_<number>}; rename the classic scenario or the parameter sheet."),
                   call = NULL)
  }
  if ("baseline" %in% scenario) {
    cli::cli_abort("A shock scenario cannot be called {.val baseline}.", call = NULL)
  }
  shock_dir <- in_project(input$shock_scenario_folder %||% calib_folder_default(), root)
  input$scenario <- scenario
  input$scenario_name <- if (length(scenario)) tolower(paste(unname(scenario), input$iso3, sep = "_")) else character(0)
  input$shocks_nb <- length(scenario)
  input$calib_scenario <- c(
    if (length(classic)) file.path(shock_dir, paste0("2_calib_shock_", classic, ".R")) else character(0),
    vapply(runs, function(r) r$file, character(1), USE.NAMES = FALSE))
  input$parametric_runs <- runs
  input
}

#' Run one shock calibration script
#'
#' @param file the script.
#' @param parent the environment it runs in: `calib_new_base`, the years, and
#'   so on.
#' @param values for a parametric scenario, the values of the parameters.
#'
#' @returns a list with `shock_ch` and `values`, the parameters the script ran
#'   with (`NULL` for a classic one).
#' @keywords internal
run_shock_sheet <- function(file, parent, values = NULL) {
  env <- new.env(parent = parent)
  if (!is.null(values)) assign(".shock_parameters", values, envir = env)
  res <- source(file, local = env)
  used <- NULL
  if (!is.null(values)) {
    used <- get0(".shock_parameters_used", envir = env, inherits = FALSE)
    if (is.null(used)) {
      cli::cli_abort("{.file {file}} never calls {.fn shock_parameters}, so the values of the grid were not used.",
                     call = NULL)
    }
  }
  shock_ch <- get0("shock_ch", envir = env, inherits = FALSE)
  if (is.null(shock_ch)) shock_ch <- res$value
  list(shock_ch = shock_ch, values = used)
}

#' The metadata of the parametric scenarios of a run
#'
#' @param runs the parametric scenarios, from [parametric_runs()].
#' @param used the values each scenario ran with, named by scenario. Defaults
#'   to the values of the grids.
#'
#' @returns a data frame: `scenario`, `shock`, `identifier`, `variation`, then
#'   one column per parameter. A value that is not a single number, string or
#'   logical is written as R code.
#' @keywords internal
parametric_metadata <- function(runs, used = NULL) {
  if (!length(runs)) return(NULL)
  as_text <- function(v) {
    if (is.atomic(v) && length(v) == 1L) as.character(v) else paste(deparse(v), collapse = " ")
  }
  rows <- lapply(names(runs), function(s) {
    r <- runs[[s]]
    vals <- used[[s]] %||% r$values
    c(list(scenario = s, shock = r$shock, identifier = r$identifier,
           variation = as.character(r$variation)),
      lapply(vals, as_text))
  })
  out <- dplyr::bind_rows(rows)
  out <- as.data.frame(lapply(out, utils::type.convert, as.is = TRUE),
                       stringsAsFactors = FALSE)
  out
}

#' Write the metadata of the parametric scenarios next to the output
#'
#' Writes `<project_name>_params.csv`. A run without parametric scenarios
#' removes the file an earlier run of the same project left, which would
#' otherwise describe scenarios the output no longer holds.
#'
#' @param meta the metadata, from [parametric_metadata()].
#' @param project_name the output file stem.
#' @param dir the output folder.
#'
#' @returns invisibly, the path.
#' @keywords internal
write_3me_params <- function(meta, project_name, dir = file.path("data", "output")) {
  file <- file.path(dir, paste0(project_name, "_params.csv"))
  if (is.null(meta) || !nrow(meta)) {
    if (file.exists(file)) {
      unlink(file)
      cli::cli_alert_info("Removed {.file {file}}: this run has no parametric scenario.")
    }
    return(invisible(file))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(meta, file)
  cli::cli_alert_success("Parameters of the {nrow(meta)} parametric scenario{?s} written to {.file {file}}")
  invisible(file)
}

#' Read the parameters of the parametric scenarios of a project
#'
#' @description A run with parametric shocks writes, next to its output
#'   databases, `<project_name>_params.csv`: one row per parametric scenario,
#'   with the values it ran with. This reads it back, e.g. to join the values
#'   onto the results by `scenario`.
#'
#' @param project_name the project name, the stem of the output files.
#' @param dir the output folder.
#'
#' @returns a data frame: `scenario`, `shock`, `identifier`, `variation`, then
#'   one column per parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' params <- read_3me_params("ct_parametric")
#' read_3me_output("ct_parametric") |> dplyr::left_join(params, by = "scenario")
#' }
read_3me_params <- function(project_name, dir = file.path("data", "output")) {
  file <- file.path(dir, paste0(project_name, "_params.csv"))
  if (!file.exists(file)) {
    cli::cli_abort(c("No {.file {file}}.",
                     "i" = "It is written by a run with {.code parametric_shocks} in its configuration."))
  }
  data.table::fread(file, data.table = FALSE)
}
