#' Solve the model in R
#'
#' @description Solves the model using the R solver rather than EViews, and
#'   returns the results already reshaped to long format.
#'
#' @param config_file list. The configuration object, as returned by
#'   [readconfig()].
#' @param before_solving_data data passed to the solver before solving.
#' @param overwrite_rcpp logical. Whether to use the compiled solver. `TRUE`
#'   picks thortwo's `sparse` backend, `FALSE` its `dense-r` one, which needs
#'   no compiler.
#' @param cnb the new-base calibration to solve against.
#'
#' @returns A long-format data.frame with columns `year`, `variable`, one
#'   column per scenario, plus `sector` and `commodity`.
#' @export
R_model_solver <- function(config_file = configuration,
                            before_solving_data = data_for_solver,
                            overwrite_rcpp = rcpp_option,
                           cnb = calib_new_base){

  # configuration <- NULL
  recompile_model <- NULL
  baseyear <- NULL
  lastyear <- NULL
  themodel <- NULL
  firstyear <- NULL
  calib_test <- NULL
  tolerance_calib_check <- NULL

  list2env(config_file,envir = environment())
  list2env(config_file$input,envir = environment())
  list2env(config_file$input$advanced_config,envir = environment())
  rcpp_option = overwrite_rcpp
  calib_new_base = cnb
  data_for_solver <- before_solving_data

  solver_dir <- file.path("src", "R_solver_files")
  ## A stable, project-local directory rather than a tempdir: thortwo keys its
  ## compile cache on the generated source, so an unchanged model costs no
  ## compilation on the second run.
  dir.create(solver_dir, recursive = TRUE, showWarnings = FALSE)

  ## The old `rcpp` flag becomes a choice of backend. `sparse` is compiled and
  ## the only viable option at ThreeME's size; `dense-r` needs no toolchain but
  ## is one to two orders of magnitude slower.
  backend <- if (isTRUE(rcpp_option)) "sparse" else "dense-r"

  if (recompile_model){
    ####### If model must be recompiled

    ### A.1 Transform model.prg file into solver syntax
    ("Translating model.prg file for the solver") |> message_sub_step()
    model_to_build <- prg_to_thor(base.year = baseyear, last.year = lastyear)
    if (length(model_to_build$warnings)) {
      translate_report(model_to_build)
      cli::cli_abort("The model translation reported problems; see the report above.",
           call = NULL)
    }

    ### A.2 Build model and save
    ("Creating the model for simulations") |> message_sub_step()
    themodel <- thortwo::thor_model(
      name         = "themodel",
      endogenous   = model_to_build$endo,
      exogenous    = model_to_build$exo,
      coefficients = model_to_build$coef,
      equations    = model_to_build$equations,
      backend      = backend,
      workdir      = solver_dir,
      verbose      = FALSE)

    ("Saving the model and dependencies for future usage") |> message_sub_step()
    thortwo::export_model(themodel, filename = file.path(solver_dir, "model_thor.txt"))
    ## Self-contained: the .rds carries the generated solver source, so it can
    ## be moved between machines and survives a cleared tempdir.
    thortwo::thor_save(themodel, file.path(solver_dir, "themodel.rds"))

    data_3me <- model_to_build$data |> filter( year %in% c(firstyear:lastyear))
    saveRDS(data_3me, file.path(solver_dir, "data_thor.rds"))

  }else{
    ### A.3 Load model and calib
    themodel <- thortwo::thor_load(file.path(solver_dir, "themodel.rds"))
    data_3me <- readRDS(file.path(solver_dir, "data_thor.rds"))
  }

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
    solved_data[[scenar_solved]] <- thortwo::thor_solve(
      themodel,
      from = baseyear, to = lastyear,
      data = data_for_solver[[scenar_solved]],
      index_time = "year", verbose = FALSE) |>
      select(year, any_of(tolower(variables_to_keep)) )

  }

  ### Generate long format datafull

  data_full <- solved_data |> imap(~pivot_longer(.x,cols = !year,names_to = "variable",values_to = .y) |>
                                      mutate(variable = toupper(variable))) |>
    reduce(full_join, by = c("year","variable")) |>
    mutate(sector = NA_character_,commodity = NA_character_) |> as.data.frame()



}
