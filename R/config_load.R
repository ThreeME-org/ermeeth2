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
#'   `shock_scenario_folder` (for the `2_calib_shock_<scenario>.R`, and the
#'   automated shocks files).
#'
#'   `rcpp_option`, the old name of `Rcpp`, is still understood.
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

  ## CHECKS HERE
  # parameters_range <- NULL ### @Anissa need to correct this !!!
  ### Check calib files exist

  ### initialisation

  project_name = NULL
  iso3 = NULL
  classification = NULL
  model_folder = NULL
  automated_shocks = NULL
  scenario_baseline = NULL
  baseline_scenario_folder = NULL
  shock_scenario_folder = NULL
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
  skip_compiler = NULL
  recompile_model = NULL
  Rcpp = NULL
  rcpp_option = NULL
  Rsolver_decompose = NULL
  Rsolver_sequential = NULL
  Rsolver_reuse_jacobian = NULL
  Rsolver_rtol = NULL
  Rsolver_atol = NULL
  Rsolver_max_iter = NULL
  Rsolver_damping = NULL
  Rsolver_verbose = NULL
  Rsolver_timings = NULL
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

  ## Compulsory options. With automated shocks the scenarios come from the
  ## parameters generator, not from the file.
  required <- config_required()
  if (isTRUE(get("automated_shocks", envir = cfg))) required <- setdiff(required, "scenario")
  missing_required <- required[!vapply(required, function(nm) {
    nm %in% set_by_file && !is.null(get(nm, envir = cfg))
  }, logical(1))]
  if (length(missing_required)) {
    cli::cli_abort(c("{.file {input_config_file}} is incomplete.",
                     "x" = "It does not set {.code {missing_required}}, which {?has/have} no default.",
                     "i" = "See {.fn config_required} for the compulsory options and {.fn config_defaults} for the others."),
                   call = NULL)
  }

  ## `rcpp_option` is the earlier name of `Rcpp`.
  if (!"Rcpp" %in% set_by_file && "rcpp_option" %in% set_by_file) {
    assign("Rcpp", get("rcpp_option", envir = cfg), envir = cfg)
    set_by_file <- c(set_by_file, "Rcpp")
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

  for (nm in c("baseline_scenario_folder", "shock_scenario_folder")) {
    folder <- get(nm)
    if (!is.character(folder) || length(folder) != 1L || is.na(folder) || !nzchar(folder)) {
      cli::cli_abort("{.code {nm}} in {.file {input_config_file}} must be a single folder path.")
    }
  }

  if (automated_shocks){
    ## if automated shocks, generate range list using
    source(file.path(shock_scenario_folder,str_c("3_automated_parameters_generator_",project_name,".R")))
    scenario <- parameters_range$name   ## if automated shocks , the names will be defined in 3_Automated_parameters_generator.R
  }

  ## Default Parameters:
  calib_baseline <- file.path(baseline_scenario_folder,paste0("1_calib_",scenario_baseline,".R"))
  calib_scenario <- file.path(shock_scenario_folder,paste0("2_calib_shock_",scenario,".R"))

  if(automated_shocks){
    calib_scenario <- file.path(shock_scenario_folder,paste0("2_calib_shock_",project_name,".R")) ## One unique scenario file will be run
  }

  scenario_name <- paste(scenario, iso3, sep = "_") |> tolower()
  shocks_nb <- length(scenario)

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
      automated_shocks = automated_shocks ,
      scenario_baseline = tolower(scenario_baseline) ,
      baseline_scenario_folder = baseline_scenario_folder,
      shock_scenario_folder = shock_scenario_folder,
      scenario = tolower(scenario) ,
      scenario_name = scenario_name,
      shocks_nb = shocks_nb ,
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
      calib_scenario = calib_scenario,

      advanced_config = list(
        Rsolver = Rsolver,
        path_eviews_exe = path_eviews_exe ,
        eviews_timeout = eviews_timeout  ,
        warning = warning ,
        tolerance_calib_check = tolerance_calib_check ,
        skip_compiler = skip_compiler,
        recompile_model = recompile_model,
        Rcpp = Rcpp,
        Rsolver_decompose = Rsolver_decompose,
        Rsolver_sequential = Rsolver_sequential,
        Rsolver_reuse_jacobian = Rsolver_reuse_jacobian,
        Rsolver_rtol = Rsolver_rtol,
        Rsolver_atol = Rsolver_atol,
        Rsolver_max_iter = Rsolver_max_iter,
        Rsolver_damping = Rsolver_damping,
        Rsolver_verbose = Rsolver_verbose,
        Rsolver_timings = Rsolver_timings,
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
#'     `scenario` is not needed with `automated_shocks = TRUE`.
#'   * `baseyear`, `lastyear`.
#'   * `lists_files`, `calib_files`, `model_files`: the `.mdl` files.
#'
#'   The defaults, besides the solver options:
#'
#'   * `iso3`, `classification`: empty, for a model with neither, such as the
#'     training model.
#'   * `automated_shocks`: `FALSE`.
#'   * `shockyear`: 2021, for now.
#'   * `max_lags`: 3. `firstyear` is not an option with a default: it is
#'     `baseyear - max_lags` unless the file sets it.
#'   * `variables_to_keep`: none, which keeps every variable.
#'   * `baseline_scenario_folder`, `shock_scenario_folder`:
#'     [calib_folder_default()].
#'   * `Rsolver`: `TRUE`, the solver that runs on every system.
#'   * `warning`: `FALSE`. `tolerance_calib_check`: 0.001.
#'   * `skip_compiler`: `FALSE`. `recompile_model`: `TRUE`.
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

#' @rdname config_required
#' @export
config_defaults <- function() {
  c(list(
    iso3                     = "",
    classification           = "",
    automated_shocks         = FALSE,
    shockyear                = 2021,
    max_lags                 = 3,
    variables_to_keep        = character(0),
    baseline_scenario_folder = calib_folder_default(),
    shock_scenario_folder    = calib_folder_default(),
    Rsolver                  = TRUE,
    warning                  = FALSE,
    tolerance_calib_check    = 1e-3,
    skip_compiler            = FALSE,
    recompile_model          = TRUE,
    save_files_res           = TRUE,
    output_saved             = character(0),
    path_eviews_exe          = "",
    eviews_timeout           = 0),
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
    Rsolver_verbose   = FALSE,
    Rsolver_timings   = TRUE,
    eviews_algorithm  = "broyden",
    eviews_digits     = 10,
    eviews_max_iter   = 5500
  )
}
