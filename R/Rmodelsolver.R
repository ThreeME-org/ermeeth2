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
#'   is loaded rather than rebuilt; `recompile_model_R = TRUE` in the
#'   configuration forces the rebuild. The remaining solver options
#'   (`Rsolver_decompose`, `Rsolver_sequential`, `Rsolver_reuse_jacobian`,
#'   `Rsolver_timings`, `Rsolver_rtol`, `Rsolver_atol`, `Rsolver_max_iter`,
#'   `Rsolver_damping`, `Rsolver_continuation`, `Rsolver_verbose`) are read
#'   from the configuration; see
#'   [config_solver_defaults()].
#'
#'   With `Rsolver_swap` set in the configuration, the scenarios it names are
#'   solved with a model in which some endogenous and exogenous variables swap
#'   roles, made from the main one by `thortwo::thor_swap()`: see
#'   [normalise_rsolver_swap()]. The baseline and every other scenario are
#'   solved with the main model as usual.
#'
#' @returns A long-format data.frame with columns `year`, `variable`, one
#'   column per scenario, plus `sector` and `commodity`.
#' @export
R_model_solver <- function(config_file = configuration,
                            before_solving_data = data_for_solver,
                            overwrite_rcpp = Rcpp,
                           cnb = calib_new_base){

  ## no "Source Cpp" tab in RStudio for the warnings of the C++ compiler
  restore_hooks <- mute_sourcecpp_hooks()
  on.exit(restore_hooks(), add = TRUE)

  # configuration <- NULL
  recompile_model_R <- NULL
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
  Rsolver_continuation <- NULL
  Rsolver_verbose <- NULL
  Rsolver_timings <- NULL
  Rsolver_swap <- NULL

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
  ## having to keep track. `recompile_model_R` forces the rebuild.
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
      recompile    = isTRUE(recompile_model_R),
      verbose      = isTRUE(Rsolver_verbose)),
      quiet_timings(thortwo::thor_model)))
  )[["elapsed"]]

  thortwo::export_model(themodel, filename = file.path(solver_dir, "model_thor.txt"))

  ### A.3 The swapped models, one per scenario that asks for one
  ##
  ## Built after the main model, from it: same equations, same backend and
  ## options, only some variables change roles. Each has a name of its own, so
  ## thortwo caches it next to the main model and a second run loads it.
  Rsolver_swap <- normalise_rsolver_swap(Rsolver_swap)
  unknown <- setdiff(names(Rsolver_swap), setdiff(names(data_for_solver), "baseline"))
  if (length(unknown)) {
    cli::cli_abort(c("{.code Rsolver_swap} names {?a scenario/scenarios} that {?is/are} not run: {.val {unknown}}.",
                     "i" = "Its names are scenario ids, as in {.code scenario}; the baseline is always solved with the main model."),
                   call = NULL)
  }
  if (length(Rsolver_swap) && !"thor_swap" %in% getNamespaceExports("thortwo")) {
    cli::cli_abort("{.code Rsolver_swap} needs a thortwo with {.fn thor_swap}: please update thortwo.",
                   call = NULL)
  }
  swapped_models <- list()
  time_swap <- system.time(
    for (scen in names(Rsolver_swap)) {
      sw <- Rsolver_swap[[scen]]
      cli::cli_alert_info("Scenario {.val {scen}}: {.val {sw$endo}} made exogenous, {.val {sw$exo}} endogenous")
      swapped_models[[scen]] <- do.call(thortwo::thor_swap, c(list(
        themodel, endo = sw$endo, exo = sw$exo,
        name      = paste0("themodel_", make.names(scen)),
        recompile = isTRUE(recompile_model_R),
        verbose   = isTRUE(Rsolver_verbose)),
        quiet_timings(thortwo::thor_model)))
    }
  )[["elapsed"]]
  if (length(swapped_models) && !isFALSE(Rsolver_timings)) {
    n_swap <- length(swapped_models)
    cli::cli_alert_info("{n_swap} swapped model{?s} ready in {format_duration(time_swap)}")
  }

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

    ## Everything is kept, except the columns the translation added for the
    ## solver (the `elem_*` coefficients standing for the `@elem` of the
    ## model): they are constants the model does not define, an EViews run has
    ## none of them, and nothing reads them back. The solve above still gets
    ## them: they are only left out of what is returned. Naming one in
    ## `variables_to_keep` keeps it.
    if(length(variables_to_keep)==0){
      ## Taken from the translation's own list, not guessed from the name: a
      ## model variable that merely has ELEM in its name is not in it. And a
      ## name that was already in the calibration is kept in any case.
      solver_only <- intersect(model_to_build$elem$name, newly_created_variables)
      variables_to_keep <- setdiff(names(data_for_solver[["baseline"]]),
                                   c("year", solver_only)) |> tolower()
    }

    scenar_solved <- scenar_order[item_scen]
    cli::cli_alert_info("Solving scenario {.val {scenar_solved}} ({item_scen}/{tot_scen})")
    # browser()
    ## No `skip_tests`: thortwo's checks are vectorised over the data matrix
    ## and cost nothing, and they name the missing variable instead of letting
    ## the solve fail twenty periods later.
    model_solved <- swapped_models[[scenar_solved]] %||% themodel
    time_solve[[scenar_solved]] <- system.time(
      solved <- withCallingHandlers(
        do.call(thortwo::thor_solve, c(list(
          model_solved,
          from = baseyear, to = lastyear,
          data = data_for_solver[[scenar_solved]],
          index_time = "year",
          rtol = Rsolver_rtol, atol = Rsolver_atol,
          max_iter = Rsolver_max_iter, damping = isTRUE(Rsolver_damping),
          ## on unless the configuration says FALSE, as in thortwo
          continuation = !isFALSE(Rsolver_continuation),
          ## "auto" leaves the choice to thortwo: on when compiled, off in pure R
          reuse_jacobian = if (is.logical(Rsolver_reuse_jacobian)) Rsolver_reuse_jacobian else NULL,
          verbose = isTRUE(Rsolver_verbose)),
          quiet_timings(thortwo::thor_solve))),
        ## The equations are passed to thortwo without names, so its messages
        ## refer to them by its own ids (`eq_2528`). Show the equations meant.
        error = function(e) show_thor_equations(conditionMessage(e), model_solved))
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

#' Check and tidy the `Rsolver_swap` option
#'
#' @description `Rsolver_swap` names the scenarios to solve with a model in
#'   which variables swap roles: for each, the endogenous variables to fix
#'   (`endo`, the targets) and the exogenous ones to solve for instead (`exo`,
#'   the instruments), as many of each. For instance, to find the carbon tax
#'   that delivers an emissions path set in the shock sheet of `co2_target`:
#'
#'   ```r
#'   Rsolver_swap = list(co2_target = list(endo = "EMS_CO2", exo = "RCO2TAX_VOL"))
#'   ```
#'
#'   The shock sheet of such a scenario sets the targets, over the whole span
#'   solved, like any exogenous variable; the instruments come out in the
#'   results. The R solver only: EViews has no equivalent here.
#'
#' @param x the value of `Rsolver_swap` in the configuration.
#'
#' @returns a named list, one `list(endo, exo)` of lower-case names per
#'   scenario; an empty list when `x` is `NULL` or empty.
#' @keywords internal
normalise_rsolver_swap <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  bad <- function(why) {
    cli::cli_abort(c("{.code Rsolver_swap} is not well formed: {why}.",
                     "i" = "Write it as {.code list(<scenario> = list(endo = \"EMS_CO2\", exo = \"RCO2TAX_VOL\"))}."),
                   call = NULL)
  }
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    bad("it must be a list named by scenario id, each name once")
  }
  lapply(stats::setNames(names(x), names(x)), function(scen) {
    sw <- x[[scen]]
    if (!is.list(sw) || !all(c("endo", "exo") %in% names(sw)) ||
        !is.character(sw$endo) || !is.character(sw$exo)) {
      bad(paste0("the entry of ", scen, " needs character vectors `endo` and `exo`"))
    }
    if (length(sw$endo) == 0L || length(sw$endo) != length(sw$exo)) {
      bad(paste0("the entry of ", scen, " needs as many `exo` as `endo`"))
    }
    list(endo = tolower(sw$endo), exo = tolower(sw$exo))
  })
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


# RStudio listens to Rcpp::sourceCpp() through two hooks, and opens a
# "Source Cpp" tab listing what the compiler said, warnings included. The code
# compiled here is generated by thortwo: its warnings are of no use to the user
# and the tab comes up at the end of every simulation that compiled. The hooks
# are emptied for the time of a solve; the function returned puts them back.
# A compilation that fails is still reported, by thortwo's own error.
mute_sourcecpp_hooks <- function() {
  hooks <- c("sourceCpp.onBuild", "sourceCpp.onBuildComplete")
  old <- lapply(hooks, getHook)
  for (h in hooks) setHook(h, list(), "replace")
  function() for (i in seq_along(hooks)) setHook(hooks[i], old[[i]], "replace")
}
