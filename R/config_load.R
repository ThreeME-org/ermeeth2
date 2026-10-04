#' Read the configuration files and create the config list
#'
#' @param input_config_file file path to the configuration input file
#' @param output_config_file file path to the configuration output file
#'
#' @details The calibration scripts of the scenarios are looked for in
#'   `baseline_scenario_folder` (for `1_calib_<scenario_baseline>.R`) and
#'   `shock_scenario_folder` (for the `2_calib_shock_<scenario>.R`, and the
#'   automated shocks files). Both default to [calib_folder_default()],
#'   `configuration/scenarii_calib`, and are meant to name a subfolder of it.
#'
#'   Solver options a configuration file does not set take the values
#'   of [config_solver_defaults()], so a file written before an option existed
#'   still reads. `rcpp_option`, the old name of `Rcpp`, is still understood.
#'
#' @returns a config list with all the necessary elements to run the simulations
#' @export
#'
#' @examples
#' \dontrun{
#' config <- readconfig <- function(
#' input_config_file = file.path("configuration", "config_input_threeme.R") ,
#' output_config_file = file.path("configuration", "config_output_threeme.R") )
#' }
#'
readconfig <- function(input_config_file = file.path("configuration", "config_input_threeme.R") ,
                       output_config_file = file.path("configuration", "config_output_threeme.R")){

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
  Rsolver_rtol = NULL
  Rsolver_atol = NULL
  Rsolver_max_iter = NULL
  Rsolver_damping = NULL
  Rsolver_verbose = NULL
  eviews_algorithm = NULL
  eviews_digits = NULL
  eviews_max_iter = NULL
  save_files_res = NULL
  path_main = NULL
  output_saved = NULL

  quartos_to_render = NULL
  quartos_parameters = NULL

  ## Read configs
  source(input_config_file,local = TRUE)
  source(output_config_file,local = TRUE)


  ## Where the calibration scripts are: scenarii_calib unless the file says
  ## otherwise.
  baseline_scenario_folder <- baseline_scenario_folder %||% calib_folder_default()
  shock_scenario_folder    <- shock_scenario_folder %||% calib_folder_default()
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

  ## Solver options: `rcpp_option` is the earlier name of `Rcpp`, and
  ## anything the file leaves out takes its default.
  if (is.null(Rcpp)) Rcpp <- rcpp_option
  solver_defaults <- config_solver_defaults()
  for (nm in names(solver_defaults)) {
    if (is.null(get(nm))) assign(nm, solver_defaults[[nm]])
  }
  eviews_algorithm <- tolower(eviews_algorithm)
  if (!eviews_algorithm %in% eviews_algorithms()) {
    cli::cli_abort(c("Unknown {.code eviews_algorithm} in {.file {input_config_file}}: {.val {eviews_algorithm}}.",
                     "i" = "Use one of {.val {unname(eviews_algorithms())}}."))
  }

  path_main <- NULL
  save_files_res <-TRUE

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
        Rsolver_rtol = Rsolver_rtol,
        Rsolver_atol = Rsolver_atol,
        Rsolver_max_iter = Rsolver_max_iter,
        Rsolver_damping = Rsolver_damping,
        Rsolver_verbose = Rsolver_verbose,
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

#' Default values of the solver options
#'
#' @description What [readconfig()] uses for a solver option the configuration
#'   file does not set, and what the configuration addin shows for it.
#'
#'   The R solver options are those of `thortwo::thor_model()` and
#'   `thortwo::thor_solve()`:
#'
#'   * `Rcpp`: `TRUE` for the compiled solver (thortwo's `sparse` backend),
#'     `FALSE` for the pure R one (`sparse-r`), which needs no compiler.
#'   * `Rsolver_decompose`: split the model into prologue, heart and epilogue.
#'   * `Rsolver_rtol`, `Rsolver_atol`: relative and absolute tolerance.
#'   * `Rsolver_max_iter`: maximum number of Newton iterations per period.
#'   * `Rsolver_damping`: damp the Newton steps.
#'   * `Rsolver_verbose`: print the solver's own progress.
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
    Rsolver_rtol      = 1e-10,
    Rsolver_atol      = 1e-8,
    Rsolver_max_iter  = 100,
    Rsolver_damping   = TRUE,
    Rsolver_verbose   = FALSE,
    eviews_algorithm  = "broyden",
    eviews_digits     = 10,
    eviews_max_iter   = 5500
  )
}
