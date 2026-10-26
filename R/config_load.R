#' Read the configuration files and create the config list
#'
#' @description Reads a configuration input file and its output file, and
#'   returns the configuration list the rest of the pipeline works from.
#'
#'   An input file does not have to set everything. A few options are
#'   **compulsory**, because no value could be guessed for them:
#'   [config_required()] lists them, and `readconfig()` stops, naming the ones
#'   missing, when a file leaves one out. Every other option has a **default**,
#'   given by [config_defaults()], which is used when the file does not set it.
#'   That is also what keeps an older file readable when an option is added.
#'
#' @details Three values are worked out when the file does not give them:
#'   `firstyear` is `baseyear - max_lags`; `quartos_to_render` and
#'   `quartos_parameters`, from the output file, are empty lists.
#'
#'   The calibration scripts of the scenarios are looked for in
#'   `baseline_scenario_folder` (for `1_calib_<scenario_baseline>.R`) and
#'   `shock_scenario_folder` (for the `2_calib_shock_<scenario>.R`).
#'
#'   **Parametric shocks.** `parametric_shocks` runs a shock sheet for several
#'   values of its parameters: `list(ct = c("redis", "traj"))` runs
#'   `2_calib_shock_ct.R` with each row of the parameter sheets
#'   `3_params_ct_redis.R` and `3_params_ct_traj.R`, read from
#'   `shock_params_folder` (the shock folder unless set). Each row is a
#'   scenario, `ct_redis_1`, `ct_redis_2`, ..., added after the classic ones
#'   of `scenario`. `scenario` may then be left out. See [shock_parameters()]
#'   and [shock_grid()].
#'
#'   Two options are still understood under the name they had before:
#'   `rcpp_option` for `Rcpp`, and `skip_compiler` for `skip_dynamo`.
#'   `recompile_model` is **not**: it was one option for both solvers and is
#'   replaced by `recompile_model_R` and `recompile_model_eviews`. A file that
#'   still sets it is told that the line is ignored.
#'
#' @param input_config_file file path to the configuration input file
#' @param output_config_file file path to the configuration output file
#' @param quiet `TRUE` to not list the options left to their default.
#'
#' @returns a config list with all the necessary elements to run the simulations
#' @export
#'
#' @examples
#' \dontrun{
#' config <- readconfig(
#'   input_config_file = file.path("configuration", "config_input_threeme.R"),
#'   output_config_file = file.path("configuration", "config_output_threeme.R"))
#' }
#'
readconfig <- function(input_config_file = file.path("configuration", "config_input_threeme.R") ,
                       output_config_file = file.path("configuration", "config_output_threeme.R"),
                       quiet = FALSE){

  ### initialisation

  project_name = NULL
  iso3 = NULL
  classification = NULL
  model_folder = NULL
  scenario_baseline = NULL
  baseline_scenario_folder = NULL
  shock_scenario_folder = NULL
  shock_params_folder = NULL
  parametric_shocks = NULL
  scenario = NULL
  scenario_name = NULL
  shocks_nb = NULL
  baseyear = NULL
  lastyear = NULL
  shockyear = NULL
  max_lags = NULL
  firstyear = NULL
  variables_to_keep = NULL
  lists_files = NULL
  calib_files = NULL
  model_files = NULL
  calib_baseline = NULL
  calib_scenario = NULL

  Rsolver = NULL
  path_eviews_exe = NULL
  eviews_timeout = NULL
  warning = NULL
  tolerance_calib_check = NULL
  skip_dynamo = NULL
  recompile_model_R = NULL
  recompile_model_eviews = NULL
  Rcpp = NULL
  rcpp_option = NULL
  Rsolver_decompose = NULL
  Rsolver_sequential = NULL
  Rsolver_reuse_jacobian = NULL
  Rsolver_rtol = NULL
  Rsolver_atol = NULL
  Rsolver_max_iter = NULL
  Rsolver_damping = NULL
  Rsolver_continuation = NULL
  Rsolver_verbose = NULL
  Rsolver_timings = NULL
  Rsolver_swap = NULL
  eviews_algorithm = NULL
  eviews_digits = NULL
  eviews_max_iter = NULL
  save_files_res = NULL
  path_main = NULL
  output_saved = NULL

  quartos_to_render = NULL
  quartos_parameters = NULL

  ## Read configs
  ##
  ## The files are sourced in an environment of their own, on top of one that
  ## holds the defaults. A file can therefore use an option it does not set
  ## (`firstyear = baseyear - max_lags` without a `max_lags`), and what the
  ## file assigned itself can be told from what was left to its default.
  defaults <- config_defaults()
  base <- list2env(defaults, envir = new.env(parent = environment()))
  cfg <- new.env(parent = base)
  source(input_config_file, local = cfg)
  set_by_file <- ls(cfg, all.names = TRUE)
  out <- new.env(parent = cfg)
  source(output_config_file, local = out)

  ## Compulsory options. With parametric shocks, classic scenarios are
  ## optional.
  required <- config_required()
  has_parametric <- length(normalise_parametric_shocks(get("parametric_shocks", envir = cfg))) > 0
  if (has_parametric) required <- setdiff(required, "scenario")
  missing_required <- required[!vapply(required, function(nm) {
    nm %in% set_by_file && !is.null(get(nm, envir = cfg))
  }, logical(1))]
  if (length(missing_required)) {
    cli::cli_abort(c("{.file {input_config_file}} is incomplete.",
                     "x" = "It does not set {.code {missing_required}}, which {?has/have} no default.",
                     "i" = "See {.fn config_required} for the compulsory options and {.fn config_defaults} for the others."),
                   call = NULL)
  }

  ## Options a file may still carry under their earlier name.
  legacy <- config_legacy_names()
  for (nm in names(legacy)) {
    if (!nm %in% set_by_file && legacy[[nm]] %in% set_by_file) {
      assign(nm, get(legacy[[nm]], envir = cfg), envir = cfg)
      set_by_file <- c(set_by_file, nm)
    }
  }

  ## Replaced, on purpose without a fallback: the one option meant different
  ## things for the two solvers, and guessing would hide that.
  if ("recompile_model" %in% set_by_file) {
    cli::cli_alert_warning("{.code recompile_model} in {.file {basename(input_config_file)}} is no longer read. Set {.code recompile_model_R} and {.code recompile_model_eviews} instead; see {.fn config_defaults}.")
  }
  if ("automated_shocks" %in% set_by_file && isTRUE(get("automated_shocks", envir = cfg))) {
    cli::cli_alert_warning("{.code automated_shocks} in {.file {basename(input_config_file)}} is no longer read. Use {.code parametric_shocks}; see {.fn shock_parameters}.")
  }

  defaulted <- setdiff(names(defaults), set_by_file)
  if (length(defaulted) && !quiet) {
    ## the wording is settled here: cli would take its plural from the file name
    n_def <- length(defaulted)
    cli::cli_alert_info("{n_def} {if (n_def == 1) 'option' else 'options'} not set in {.file {basename(input_config_file)}}, left to {if (n_def == 1) 'its' else 'their'} default:")
    cli_vector(defaulted)
  }

  for (nm in union(names(defaults), set_by_file)) {
    assign(nm, get(nm, envir = cfg))
  }
  if (!"firstyear" %in% set_by_file) firstyear <- baseyear - max_lags
  quartos_to_render  <- if (exists("quartos_to_render", envir = out, inherits = FALSE)) get("quartos_to_render", envir = out) else list()
  quartos_parameters <- if (exists("quartos_parameters", envir = out, inherits = FALSE)) get("quartos_parameters", envir = out) else list()

  ## The parameter sheets are with the shock sheets unless said otherwise.
  if (is.null(shock_params_folder)) shock_params_folder <- shock_scenario_folder

  for (nm in c("baseline_scenario_folder", "shock_scenario_folder", "shock_params_folder")) {
    folder <- get(nm)
    if (!is.character(folder) || length(folder) != 1L || is.na(folder) || !nzchar(folder)) {
      cli::cli_abort("{.code {nm}} in {.file {input_config_file}} must be a single folder path.")
    }
  }

  ## Each row of each parameter sheet is a scenario. The sheets run with the
  ## values of the configuration in scope (`shockyear`, `lastyear`, ...).
  parametric_shocks <- normalise_parametric_shocks(parametric_shocks)
  runs <- parametric_runs(parametric_shocks, shock_scenario_folder,
                          shock_params_folder, env = cfg)
  if (length(runs) && !quiet) {
    sheets <- vapply(runs, function(r) paste(r$shock, r$identifier, sep = "_"), character(1))
    per_sheet <- table(factor(sheets, levels = unique(sheets)))
    cli::cli_alert_info("Parametric shocks: {length(runs)} scenario{?s}, {paste0(names(per_sheet), ' (', per_sheet, ')', collapse = ', ')}.")
  }

  calib_baseline <- file.path(baseline_scenario_folder,paste0("1_calib_",scenario_baseline,".R"))

  ## "auto", TRUE or FALSE; a file may also write it as "true" / "false".
  if (is.character(Rsolver_reuse_jacobian) && length(Rsolver_reuse_jacobian) == 1L &&
      tolower(Rsolver_reuse_jacobian) %in% c("true", "false")) {
    Rsolver_reuse_jacobian <- tolower(Rsolver_reuse_jacobian) == "true"
  }
  if (!(isTRUE(Rsolver_reuse_jacobian) || isFALSE(Rsolver_reuse_jacobian) ||
        identical(tolower(Rsolver_reuse_jacobian), "auto"))) {
    cli::cli_abort(c("Unknown {.code Rsolver_reuse_jacobian} in {.file {input_config_file}}: {.val {Rsolver_reuse_jacobian}}.",
                     "i" = "Use {.val auto}, {.code TRUE} or {.code FALSE}."))
  }
  if (is.character(Rsolver_reuse_jacobian)) Rsolver_reuse_jacobian <- "auto"
  Rsolver_swap <- normalise_rsolver_swap(Rsolver_swap)
  if (length(Rsolver_swap) && isFALSE(Rsolver)) {
    cli::cli_abort(c("{.code Rsolver_swap} is set in {.file {input_config_file}}, but {.code Rsolver = FALSE}.",
                     "i" = "Swapping endogenous and exogenous variables is done by the R solver only: set {.code Rsolver = TRUE}."),
                   call = NULL)
  }
  eviews_algorithm <- tolower(eviews_algorithm)
  if (!eviews_algorithm %in% eviews_algorithms()) {
    cli::cli_abort(c("Unknown {.code eviews_algorithm} in {.file {input_config_file}}: {.val {eviews_algorithm}}.",
                     "i" = "Use one of {.val {unname(eviews_algorithms())}}."))
  }

  path_main <- NULL

  config <- list(
    input = list(
      project_name = project_name,
      iso3 = tolower(iso3),
      classification = tolower(classification),
      model_folder = model_folder,
      scenario_baseline = tolower(scenario_baseline) ,
      baseline_scenario_folder = baseline_scenario_folder,
      shock_scenario_folder = shock_scenario_folder,
      shock_params_folder = shock_params_folder,
      parametric_shocks = parametric_shocks,
      baseyear = baseyear,
      lastyear = lastyear,
      shockyear = shockyear,
      max_lags = max_lags,
      firstyear = firstyear,
      variables_to_keep = variables_to_keep,
      lists_files = lists_files,
      calib_files = calib_files,
      model_files = model_files,
      calib_baseline = calib_baseline,

      advanced_config = list(
        Rsolver = Rsolver,
        path_eviews_exe = path_eviews_exe ,
        eviews_timeout = eviews_timeout  ,
        warning = warning ,
        tolerance_calib_check = tolerance_calib_check ,
        skip_dynamo = skip_dynamo,
        recompile_model_R = recompile_model_R,
        recompile_model_eviews = recompile_model_eviews,
        Rcpp = Rcpp,
        Rsolver_decompose = Rsolver_decompose,
        Rsolver_sequential = Rsolver_sequential,
        Rsolver_reuse_jacobian = Rsolver_reuse_jacobian,
        Rsolver_rtol = Rsolver_rtol,
        Rsolver_atol = Rsolver_atol,
        Rsolver_max_iter = Rsolver_max_iter,
        Rsolver_damping = Rsolver_damping,
        Rsolver_continuation = Rsolver_continuation,
        Rsolver_verbose = Rsolver_verbose,
        Rsolver_timings = Rsolver_timings,
        Rsolver_swap = Rsolver_swap,
        eviews_algorithm = eviews_algorithm,
        eviews_digits = eviews_digits,
        eviews_max_iter = eviews_max_iter,
        save_files_res = save_files_res,
        path_main = path_main,
        output_saved = output_saved)


      ),
    output = list(
      quartos_to_render = quartos_to_render ,
      quartos_parameters = quartos_parameters
    )
  )

  ## scenario, scenario_name, shocks_nb, calib_scenario, parametric_runs
  config$input <- config_scenarios(config$input, scenario, runs)
  config
}

#' The options a configuration file has to set, and the defaults of the others
#'
#' @description `config_required()` names the options of a configuration input
#'   file that have no default: [readconfig()] stops when one is missing.
#'
#'   `config_defaults()` gives the value of every other option, used when a
#'   file does not set it. It includes the solver options of
#'   [config_solver_defaults()].
#'
#'   Between them they hold every option a configuration input file can set,
#'   and the configuration addin ([config_addin()]) offers each of them.
#'
#' @details The compulsory options:
#'
#'   * `project_name`: the stem of the output files.
#'   * `model_folder`: the folder of `src/model` the model is in.
#'   * `scenario_baseline`, `scenario`: the baseline and the shocks to run.
#'     `scenario` is not needed when `parametric_shocks` names at least one
#'     parameter sheet.
#'   * `baseyear`, `lastyear`.
#'   * `lists_files`, `calib_files`, `model_files`: the `.mdl` files.
#'
#'   The defaults, besides the solver options:
#'
#'   * `iso3`, `classification`: empty, for a model with neither, such as the
#'     training model.
#'   * `parametric_shocks`: none. Otherwise a list naming, for each shock
#'     sheet, the parameter sheets to run it with:
#'     `list(ct = c("redis", "traj"))`. See [shock_parameters()].
#'   * `shockyear`: 2021, for now.
#'   * `max_lags`: 3. `firstyear` is not an option with a default: it is
#'     `baseyear - max_lags` unless the file sets it.
#'   * `variables_to_keep`: none, which keeps every variable.
#'   * `baseline_scenario_folder`, `shock_scenario_folder`:
#'     [calib_folder_default()].
#'   * `shock_params_folder`, the folder of the parameter sheets: `NULL`,
#'     which is `shock_scenario_folder`, whatever that is set to.
#'   * `Rsolver`: `TRUE`, the solver that runs on every system.
#'   * `warning`: `FALSE`. `tolerance_calib_check`: 0.001.
#'   * `skip_dynamo`: `FALSE`, so DynaMo compiles the `.mdl` files on every
#'     run. An advanced option, to leave alone without a good reason: `TRUE`
#'     reuses the `model.prg` and `calib.csv` already in `src/compiler`, so
#'     any change to the `.mdl` files is ignored, and neither solver can tell.
#'     A run with `TRUE` says so.
#'   * `recompile_model_R`: `FALSE`. The R solver then reuses the model it
#'     built before when the equations have not changed, and rebuilds it when
#'     they have, so `FALSE` is always safe. `TRUE` forces the rebuild.
#'   * `recompile_model_eviews`: `TRUE`. EViews then loads the model, the
#'     calibration and the baseline afresh. `FALSE` reopens the workfile an
#'     earlier run saved, without reloading any of the three: only for a
#'     rerun where none of them has changed.
#'   * `save_files_res`: `TRUE`.
#'   * `output_saved`: none, so no aggregated database is saved.
#'   * `path_eviews_exe`: empty, which makes [eviews_checks()] search the
#'     usual locations. `eviews_timeout`: 0, no time out.
#'
#' @returns `config_required()` returns a character vector, `config_defaults()`
#'   a named list.
#' @export
#'
#' @examples
#' config_required()
#' str(config_defaults())
config_required <- function() {
  c("project_name", "model_folder", "scenario_baseline", "scenario",
    "baseyear", "lastyear",
    "lists_files", "calib_files", "model_files")
}

#' Options that had another name
#'
#' @returns a character vector: the earlier name of an option, named by the
#'   current one. [readconfig()] and the configuration addin read a file that
#'   still uses the earlier name.
#' @keywords internal
config_legacy_names <- function() {
  c(Rcpp = "rcpp_option", skip_dynamo = "skip_compiler")
}

#' @rdname config_required
#' @export
config_defaults <- function() {
  c(list(
    iso3                     = "",
    classification           = "",
    parametric_shocks        = list(),
    shockyear                = 2021,
    max_lags                 = 3,
    variables_to_keep        = character(0),
    baseline_scenario_folder = calib_folder_default(),
    shock_scenario_folder    = calib_folder_default(),
    shock_params_folder      = NULL,
    Rsolver                  = TRUE,
    warning                  = FALSE,
    tolerance_calib_check    = 1e-3,
    skip_dynamo              = FALSE,
    recompile_model_R        = FALSE,
    recompile_model_eviews   = TRUE,
    save_files_res           = TRUE,
    output_saved             = character(0),
    path_eviews_exe          = "",
    eviews_timeout           = 0,
    ## a list: edited in the configuration file, not in the addin
    Rsolver_swap             = list()),
    config_solver_defaults())
}

#' Default values of the solver options
#'
#' @description What [readconfig()] uses for a solver option the configuration
#'   file does not set, and what the configuration addin shows for it. They
#'   are part of [config_defaults()].
#'
#'   The R solver options are those of `thortwo::thor_model()` and
#'   `thortwo::thor_solve()`:
#'
#'   * `Rcpp`: `TRUE` for the compiled solver (thortwo's `sparse` backend),
#'     `FALSE` for the pure R one (`sparse-r`), which needs no compiler.
#'   * `Rsolver_decompose`: split the model into prologue, heart and epilogue.
#'   * `Rsolver_sequential`: solve the prologue and the epilogue one equation
#'     at a time, in dependency order, instead of by Newton. Same solution and
#'     about the same speed; its use is the error message, which names the
#'     equation and the variable that cannot be determined.
#'   * `Rsolver_reuse_jacobian`: keep the factorised jacobian from one Newton
#'     iteration to the next within a period. `"auto"` lets thortwo decide (on
#'     with `Rcpp = TRUE`, where it makes a large model about three times
#'     faster to solve; off in pure R), `TRUE` / `FALSE` force it. `FALSE` is
#'     plain Newton.
#'   * `Rsolver_rtol`, `Rsolver_atol`: relative and absolute tolerance.
#'   * `Rsolver_max_iter`: maximum number of Newton iterations per period.
#'   * `Rsolver_damping`: damp the Newton steps.
#'   * `Rsolver_continuation`: when a period does not solve from the previous
#'     period's solution, solve it again with the exogenous variables moved
#'     there in steps. Periods that solve directly are not affected. `FALSE`
#'     solves each period once.
#'   * `Rsolver_verbose`: print the solver's own progress.
#'   * `Rsolver_timings`: print, at the end of the solve, how long the
#'     translation, the build, the compilation and each scenario took.
#'
#'   The EViews ones are written into `src/EViews/solve.prg` by
#'   [eviews_solve_options()]:
#'
#'   * `eviews_algorithm`: `"broyden"`, `"newton"` or `"gauss-seidel"`.
#'   * `eviews_digits`: number of digits the solution is rounded to.
#'   * `eviews_max_iter`: maximum number of iterations.
#'
#' @returns a named list.
#' @export
#'
#' @examples
#' config_solver_defaults()
config_solver_defaults <- function() {
  list(
    Rcpp              = TRUE,
    Rsolver_decompose = TRUE,
    Rsolver_sequential = FALSE,
    Rsolver_reuse_jacobian = "auto",
    Rsolver_rtol      = 1e-10,
    Rsolver_atol      = 1e-8,
    Rsolver_max_iter  = 100,
    Rsolver_damping   = TRUE,
    Rsolver_continuation = TRUE,
    Rsolver_verbose   = FALSE,
    Rsolver_timings   = TRUE,
    eviews_algorithm  = "broyden",
    eviews_digits     = 10,
    eviews_max_iter   = 5500
  )
}
