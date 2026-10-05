#' Solve the model in R
#'
#' @description Solves the model using the R solver rather than EViews, and
#'   returns the results already reshaped to long format.
#'
#' @param config_file list. The configuration object, as returned by
#'   [readconfig()].
#' @param before_solving_data data passed to the solver before solving.
#' @param overwrite_rcpp logical. Whether to use the compiled solver. `TRUE`
#'   picks thortwo's `sparse` backend, `FALSE` its `sparse-r` one, which is
#'   pure R and needs no compiler.
#' @param cnb the new-base calibration to solve against.
#'
#' @details The model is built by `thortwo::thor_model()` on every call.
#'   thortwo caches what it builds, so a model whose equations have not changed
#'   is loaded rather than rebuilt; `recompile_model = TRUE` in the
#'   configuration forces the rebuild. The remaining solver options
#'   (`Rsolver_decompose`, `Rsolver_sequential`, `Rsolver_reuse_jacobian`,
#'   `Rsolver_timings`, `Rsolver_rtol`, `Rsolver_atol`, `Rsolver_max_iter`,
#'   `Rsolver_damping`, `Rsolver_verbose`) are read from the configuration; see
#'   [config_solver_defaults()].
#'
#' @returns A long-format data.frame with columns `year`, `variable`, one
#'   column per scenario, plus `sector` and `commodity`.
#' @export
R_model_solver <- function(config_file = configuration,
                            before_solving_data = data_for_solver,
                            overwrite_rcpp = Rcpp,
                           cnb = calib_new_base){

  # configuration <- NULL
  recompile_model <- NULL
  baseyear <- NULL
  lastyear <- NULL
  themodel <- NULL
  firstyear <- NULL
  calib_test <- NULL
  tolerance_calib_check <- NULL
  Rsolver_decompose <- NULL
  Rsolver_sequential <- NULL
  Rsolver_reuse_jacobian <- NULL
  Rsolver_rtol <- NULL
  Rsolver_atol <- NULL
  Rsolver_max_iter <- NULL
  Rsolver_damping <- NULL
  Rsolver_verbose <- NULL
  Rsolver_timings <- NULL

  list2env(config_file,envir = environment())
  list2env(config_file$input,envir = environment())
  list2env(config_file$input$advanced_config,envir = environment())
  calib_new_base = cnb
  data_for_solver <- before_solving_data

  solver_dir <- file.path("src", "R_solver_files")
  dir.create(solver_dir, recursive = TRUE, showWarnings = FALSE)

  ## `Rcpp` is a choice of backend, both sparse: `sparse` is compiled,
  ## `sparse-r` is pure R and needs no toolchain. thortwo's third backend,
  ## `dense-cpp`, is never faster than `sparse` and is not offered.
  backend <- if (isTRUE(overwrite_rcpp)) "sparse" else "sparse-r"

  ### A.1 Transform model.prg file into solver syntax
  ("Translating model.prg file for the solver") |> message_sub_step()
  ## thortwo never sees the translation, so it is timed here.
  time_translation <- system.time(
    model_to_build <- prg_to_thor(base.year = baseyear, last.year = lastyear)
  )[["elapsed"]]
  if (length(model_to_build$warnings)) {
    translate_report(model_to_build)
    cli::cli_abort("The model translation reported problems; see the report above.",
         call = NULL)
  }

  ### A.2 Build the model
  ##
  ## Called every time: thortwo keys its cache on the model itself (variables,
  ## equations, backend, decomposition), so an unchanged model is loaded in a
  ## fraction of a second and an edited one is rebuilt, without this function
  ## having to keep track. `recompile_model` forces the rebuild.
  ("Creating the model for simulations") |> message_sub_step()
  ## The timings are printed once, at the end, by this function: thortwo's own
  ## lines are switched off, where the installed thortwo has the argument.
  quiet_timings <- function(fn) if ("timings" %in% names(formals(fn))) list(timings = FALSE) else list()
  time_model <- system.time(
    themodel <- do.call(thortwo::thor_model, c(list(
      name         = "themodel",
      endogenous   = model_to_build$endo,
      exogenous    = model_to_build$exo,
      coefficients = model_to_build$coef,
      equations    = model_to_build$equations,
      backend      = backend,
      decompose    = isTRUE(Rsolver_decompose),
      sequential   = isTRUE(Rsolver_sequential),
      recompile    = isTRUE(recompile_model),
      verbose      = isTRUE(Rsolver_verbose)),
      quiet_timings(thortwo::thor_model)))
  )[["elapsed"]]

  thortwo::export_model(themodel, filename = file.path(solver_dir, "model_thor.txt"))

  data_3me <- model_to_build$data |> filter( year %in% c(firstyear:lastyear))

  ### A.4 Consolidating databases : adding the newly created variables elem variables to calib_new_base

  #### if new variables created for the Rsolver must be added to calib_new_base
  newly_created_variables <- setdiff(names(data_3me), names(calib_new_base) )
  ## They must also reach every scenario's database, which is what the solver
  ## actually reads: the `@elem` values and coefficients are not in calib.csv.
  if(length(newly_created_variables) > 0){
    new_vars_data <- data_3me |>
      select(all_of(c("year", newly_created_variables)))
    calib_new_base <- calib_new_base |>
      full_join(new_vars_data, by = "year" )
    data_for_solver <- data_for_solver |>
      map(~ .x |>
            select(-any_of(newly_created_variables)) |>
            left_join(new_vars_data, by = "year"))
  }




  ### A.5 Check calibration at baseyear for calib.csv
  ##
  ## This used to reach into the model object's `@prologue`,
  ## `@prologue_equations_f` and `@equation_list` slots and evaluate each
  ## block's residual function by hand. thortwo's class has no such slots --
  ## and the R closures do not exist at all on a compiled backend -- so the
  ## check now goes through `calibration_check()`, which evaluates every
  ## equation through the same generated code the solver uses and names the
  ## ones that are off.
  ("Calibration check at the base year with the calibration data") |> message_sub_step()

  equations_check <- thortwo::calibration_check(
    themodel, data_3me, period = baseyear, index_time = "year",
    tolerance = tolerance_calib_check)

  if(nrow(equations_check) == 0 ){
    cli::cli_alert_success("All equations are well calibrated at the base year with {.file calib.csv}.")
  }else{
    cli::cli_alert_warning("{nrow(equations_check)} equation{?s} {?is/are} not well calibrated for the baseline scenario:")
    for (k in seq_len(nrow(equations_check))) {
      cli::cli_bullets(c("*" = "{.field {equations_check$equation[k]}} ({equations_check$part[k]}), residual {signif(equations_check$residual[k], 3)}"))
      cli::cli_verbatim(paste0("    ", equations_check$formula[k]))
    }

    #### TODO : ADD OPTION / PROMPT to stop here
  }


  ##solver

  solved_data <- data_for_solver
  ## 1. Solving using R : loop needed while we work on parallelisation
  "Solving each scenario, please wait... \U23F1" |>   message_main_step()
  scenar_order <- c("baseline", setdiff(names(data_for_solver), "baseline"))
  tot_scen <- length(scenar_order)
  time_solve <- stats::setNames(numeric(tot_scen), scenar_order)


  for (item_scen in 1:tot_scen){

    if(length(variables_to_keep)==0){
      variables_to_keep <- setdiff(names(data_for_solver[["baseline"]]), "year") |> tolower()
    }

    scenar_solved <- scenar_order[item_scen]
    cli::cli_alert_info("Solving scenario {.val {scenar_solved}} ({item_scen}/{tot_scen})")
    # browser()
    ## No `skip_tests`: thortwo's checks are vectorised over the data matrix
    ## and cost nothing, and they name the missing variable instead of letting
    ## the solve fail twenty periods later.
    time_solve[[scenar_solved]] <- system.time(
      solved <- withCallingHandlers(
        do.call(thortwo::thor_solve, c(list(
          themodel,
          from = baseyear, to = lastyear,
          data = data_for_solver[[scenar_solved]],
          index_time = "year",
          rtol = Rsolver_rtol, atol = Rsolver_atol,
          max_iter = Rsolver_max_iter, damping = isTRUE(Rsolver_damping),
          ## "auto" leaves the choice to thortwo: on when compiled, off in pure R
          reuse_jacobian = if (is.logical(Rsolver_reuse_jacobian)) Rsolver_reuse_jacobian else NULL,
          verbose = isTRUE(Rsolver_verbose)),
          quiet_timings(thortwo::thor_solve))),
        ## The equations are passed to thortwo without names, so its messages
        ## refer to them by its own ids (`eq_2528`). Show the equations meant.
        error = function(e) show_thor_equations(conditionMessage(e), themodel))
    )[["elapsed"]]
    solved_data[[scenar_solved]] <- solved |>
      select(year, any_of(tolower(variables_to_keep)) )

  }

  if (!isFALSE(Rsolver_timings)) {
    show_solver_timings(translation = time_translation, model = time_model,
                        build = tryCatch(themodel@meta$timings, error = function(e) NULL),
                        solve = time_solve)
  }

  ### Generate long format datafull

  data_full <- solved_data |> imap(~pivot_longer(.x,cols = !year,names_to = "variable",values_to = .y) |>
                                      mutate(variable = toupper(variable))) |>
    reduce(full_join, by = c("year","variable")) |>
    mutate(sector = NA_character_,commodity = NA_character_) |> as.data.frame()



}

#' Print the equations a thortwo error message refers to
#'
#' @description ermeeth2 passes the equations to thortwo without names, so
#'   thortwo names them itself (`eq_1`, `eq_2`, ...) and its error messages use
#'   those ids. This prints the text of each equation a message mentions, so
#'   that "equation 'eq_2528' could not be solved for 'pk_sgzx'" can be read.
#'   The error itself is left to propagate.
#'
#' @param msg the error message.
#' @param model the `thor_model`.
#'
#' @returns invisibly, the equations found, named by id.
#' @keywords internal
show_thor_equations <- function(msg, model) {
  ids <- unique(regmatches(msg, gregexpr("\\beq_[0-9]+\\b", msg))[[1]])
  eq <- tryCatch(model@equations, error = function(e) NULL)
  if (!length(ids) || is.null(eq)) return(invisible(character(0)))
  found <- stats::setNames(eq$equation[match(ids, eq$name)], ids)
  found <- found[!is.na(found)]
  for (id in names(found)) {
    cli::cli_alert_danger("Equation {.field {id}}:")
    cli::cli_verbatim(paste0("    ", found[[id]]))
  }
  invisible(found)
}

#' A duration, in the unit that reads best
#'
#' @param seconds a number of seconds.
#'
#' @returns a string: `"0.52 s"`, `"38.4 s"`, `"1 min 14 s"`.
#' @keywords internal
format_duration <- function(seconds) {
  vapply(seconds, function(x) {
    if (is.na(x)) return("n/a")
    if (x < 1) return(paste0(format(round(x, 2), nsmall = 2), " s"))
    if (round(x, 1) < 60) return(paste0(format(round(x, 1), nsmall = 1), " s"))
    paste0(round(x) %/% 60, " min ", round(x) %% 60, " s")
  }, character(1))
}

#' Print how long each stage of the R solver took
#'
#' @param translation seconds spent in [prg_to_thor()].
#' @param model seconds spent in `thortwo::thor_model()`, timed from outside.
#' @param build what thortwo reports for that call (`model@meta$timings`): a
#'   named vector with `build` and `compile`, and an attribute `from_cache`.
#'   `NULL` with a thortwo that does not report it; `model` is then shown as
#'   one figure.
#' @param solve seconds spent solving each scenario, named by scenario.
#'
#' @returns invisibly, the two lines printed.
#' @keywords internal
show_solver_timings <- function(translation, model, build = NULL, solve = numeric(0)) {
  stages <- paste("translation", format_duration(translation))
  if (is.numeric(build) && all(c("build", "compile") %in% names(build))) {
    cached <- isTRUE(attr(build, "from_cache"))
    stages <- c(stages,
                paste0("build ", format_duration(build[["build"]]), if (cached) " (from the cache)"),
                if (!is.na(build[["compile"]])) paste("compile", format_duration(build[["compile"]])))
  } else {
    stages <- c(stages, paste("build and compile", format_duration(model)))
  }
  lines <- c(paste(stages, collapse = " | "),
             if (length(solve)) paste0("solve: ", paste(names(solve), format_duration(solve), collapse = " | ")))
  cli::cli_alert_info("Timings: {cli_escape(lines[1])}")
  if (length(lines) > 1L) cli::cli_verbatim(paste0("  ", lines[-1]))
  invisible(lines)
}
