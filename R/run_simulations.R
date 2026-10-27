#' Run a full set of ThreeME simulations
#'
#' @description End-to-end driver: compiles the model with DynaMo, runs the
#'   configuration checks, sources the scenario calibrations, solves each
#'   scenario with the configured solver, and saves the output datasets to
#'   `data/output`.
#'
#' @param configuration list. The configuration object, as returned by
#'   [readconfig()].
#' @param export_scenarii_to_excel logical. Whether to also write the scenario
#'   results out to Excel.
#' @param output_format character vector. Which formats the output databases are
#'   written in: `"parquet"` (the default), `"rds"`, or both. Parquet files are
#'   read back with column and row pushdown by [read_3me_output()], which is far
#'   cheaper than loading a whole `.rds` to filter it afterwards. Add `"rds"`
#'   only if something still calls `readRDS()` on these files;
#'   [read_3me_output()] reads either format.
#'
#' @returns The main long-format results data.frame. The aggregate, commodity
#'   and sector datasets are written to `data/output` as a side effect,
#'   according to the `output_saved` configuration entry. With parametric
#'   shocks, the values each parametric scenario ran with are written there
#'   too, as `<project_name>_params.csv` (see [read_3me_params()]).
#'
#'   With the R solver, a scenario with a period that cannot be solved stops
#'   the run with an error, after keeping the results up to the failure as
#'   `data_full_FAILED` and `<project_name>_FAILED` in `data/output`: see
#'   [stop_failed_run()].
#' @export
run_simulations <- function(configuration = config,
                            export_scenarii_to_excel = FALSE,
                            output_format = "parquet"){

  output_format <- match.arg(output_format, c("rds", "parquet"), several.ok = TRUE)

  # config <- NULL
  input <- NULL
  baseyear <- NULL
  baseline_ch <- NULL
  firstyear <- NULL
  lastyear <- NULL
  Rsolver <- NULL
  calib_scenario <- NULL
  parametric_runs <- NULL
  scenario <- NULL
  output_saved <- NULL
  V1 <- NULL
  V2 <- NULL
  project_name <- NULL


  list2env(configuration,envir = environment())
  list2env(input,envir = environment())
  list2env(advanced_config,envir = environment())
  compil = "dynamo"
  ### PART 1 RUNNING DYNAMO

  run_dynamo(config_list = configuration)

  post_compiler_checks(base_advanced_arguments = advanced_config )
  new_params_solver<- eviews_checks(config_list = configuration)

  list2env(new_params_solver,envir = environment())

  ### PART 2 SOURCING SCENARII CALIBRATION

  calib <- fread("src/compiler/calib.csv", data.table = FALSE) |>
    select(-all_of("baseyear")) |>
    mutate(year = year + baseyear)

  OGcalib <- calib

  ## B Baseline shock calibration (ie deviating from the steady state at the baseline configuration)
  source(calib_baseline, local = TRUE)
  write.csv(baseline_ch,
            file = file.path("configuration","scenarii_calib","calib_baseline.csv"),
            row.names = FALSE)

  ## A scenario is not expected to touch the base year, which is the
  ## calibration (and the year every @elem of the model is read at).
  endogenous_vars <- readLines(file.path("src", "compiler", "model.prg")) |>
    str_remove_all("^a_3ME\\.append\\s") |>
    model_prg_endogenous()
  ## The years before it are only read as lags, never solved, so there every
  ## variable counts.
  check_calibration_years <- function(changed, reference, what) {
    base_year_changes(changed, reference, baseyear, endogenous_vars) |>
      warn_base_year_changes(what, baseyear)
    lag_year_changes(changed, reference, baseyear) |>
      warn_base_year_changes(what, baseyear, before = TRUE)
  }
  check_calibration_years(baseline_ch, OGcalib,
                          paste0("The baseline (", scenario_baseline, ")"))

  ## C Modified calib to integrate potential baseline changes

  baseline_vars <- setdiff(names(baseline_ch), "year")

  calib_new_base <- OGcalib |>
    filter(year %in% c(firstyear:lastyear)) |>
    select(-all_of(baseline_vars)) |>
    full_join(baseline_ch, by= "year")
  ### calib_new_base now integrates changes with the baseline scenario and should be used to configure shock scenarii

  ## D Scenarii shock calibration
  ## Each scenario runs its script (the shock sheet, for a parametric one) in
  ## an environment of its own on top of this one. Every script is run before
  ## stopping, so that one failure does not hide the others.
  sim_env <- environment()
  shock_files <- set_names(calib_scenario, unname(scenario))
  shock_runs <- shock_files |>
    imap(function(file, s) {
      tryCatch(run_shock_sheet(file, sim_env, parametric_runs[[s]]$values),
               error = function(e) e)
    })
  failed <- keep(shock_runs, ~inherits(.x, "error"))
  if (length(failed)) {
    cli::cli_abort(c("{length(failed)} scenario{?s} could not be calibrated:",
                     set_names(paste0(names(failed), ": ", purrr::map_chr(failed, conditionMessage)), rep("x", length(failed)))),
                   call = NULL)
  }
  shock_ch_scenarii <- map(shock_runs, "shock_ch")
  params_meta <- parametric_metadata(parametric_runs,
                                     used = map(shock_runs, "values") |> purrr::compact())

  shock_ch_scenarii |>
    purrr::iwalk(~ check_calibration_years(.x, calib_new_base, paste0("Scenario ", .y)))

  if(Rsolver== FALSE){
    shock_ch_scenarii |> imap(~write.csv(.x,
                                          file = file.path("configuration","scenarii_calib",str_c("calib_shock_",.y,".csv")),
                                          row.names = FALSE))}

  all_scenarii <- append(shock_ch_scenarii,  list(baseline_ch) ) |> set_names(c(scenario, "baseline"))


  ## E. Saving all scenarii to an Excel spreadsheet (it is only for ex-post checking purposes)
  ##### START  EXCEL SCENARIO SHOCK SAVE
  if(export_scenarii_to_excel){

  ### Save a summary of all scenarii to Excel
  xlsx.file = "configuration/scenarii_calib/summary_of_scenarii.xlsx"
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::saveWorkbook(wb, xlsx.file, overwrite = TRUE)

  ### Preparing the workbook
  wb <- openxlsx::loadWorkbook(xlsx.file)

  all_scenarii |> imap(function(database, name) {
    if (!name %in% sheets(wb)){
      openxlsx::addWorksheet(wb,sheetName = name)
    }else{
      openxlsx::removeWorksheet(wb,name)
      openxlsx::addWorksheet(wb,sheetName = name)
    }

    n_cols <- ncol(database) - 1

    openxlsx::writeData(wb , t(database),    sheet = name, startCol = "B", startRow = 1, colNames = FALSE, rowNames = TRUE)
    openxlsx::writeData(wb , "" ,            sheet = name, startCol = "B", startRow = 1, colNames = FALSE, rowNames = FALSE)
    openxlsx::writeData(wb , n_cols ,        sheet = name, startCol = "A", startRow = 1, colNames = FALSE, rowNames = FALSE)
    openxlsx::writeData(wb , rep(1,n_cols) , sheet = name, startCol = "A", startRow = 2, colNames = FALSE, rowNames = FALSE)

  })

  ### saving
  openxlsx::saveWorkbook(wb,xlsx.file,overwrite = TRUE)
  }
  ##### END EXCEL SCENARIO SHOCK SAVE


  data_for_solver <- all_scenarii |>
    map(~update_data_merge(calib_new_base , .x)) ##data_for solver contains the full databases needed to run the solver for each scenario

  ### PART 3 MODEL BUILDING AND SOLVING
  ### Prepare and store the databases in long format from EViews

  reagg_bool  = ifelse(length(intersect(output_saved, c("com","sec","sec_com") )) >0 , TRUE, FALSE )

  if(file.exists(paste0("src/bridges/bridge_",classification,".R")) & file.exists(paste0("src/bridges/codenames_",classification,".R"))){

    # source("src/functions_src/0_loadResults.R")
    source(paste0("src/bridges/bridge_",classification,".R"),local = TRUE)
    source(paste0("src/bridges/codenames_",classification,".R"),local = TRUE)
    bridge_check = TRUE
  }else{
    bridge_check = FALSE
    bridge_sectors = NULL
    bridge_commodities = NULL


    ### Commodities: c4
    names_commodities <- rbind(
      c('Industry','cind'),
      c('Transport','ctrp'),
      c('Services','cser'),
      c('Energy','cenj'),
      c('Industry','C001'),
      c('Transport','C002'),
      c('Services','C003'),
      c('Energy','C004')
    ) |>
      as.data.frame() |> rename(name = V1,code = V2)

    ### Sectors: s4
    names_sectors <- rbind(
      c('Industry','sind'),
      c('Transport','strp'),
      c('Services','sser'),
      c('Energy','senj'),
      c('Industry','s001'),
      c('Transport','s002'),
      c('Services','s003'),
      c('Energy','s004')
    ) |>
      as.data.frame() |> rename(name = V1,code = V2)

  }

  #### 3.A R SOLVER

  if(Rsolver){

    data_full <- R_model_solver(
      config_file = configuration,
      before_solving_data = data_for_solver,
      ## the value reconciled by eviews_checks(), not the raw config one
      overwrite_rcpp = Rcpp,
      cnb = calib_new_base
      )
    ## a scenario that could not be solved: keep what there is, and stop
    if (!is.null(attr(data_full, "failure"))) {
      stop_failed_run(data_full, project_name, formats = output_format)
    }

    data_list <-  data_full |>
      aggregate_com_sec(bridge_com = bridge_commodities ,bridge_sec = bridge_sectors ,by_com = reagg_bool ,by_sec = reagg_bool , scenarios =  c("baseline",scenario |> unname())) |> purrr::compact() |>
      map(~as.data.frame(.x) |> add_com_sec_names(commodities_names = names_commodities,sectors_names = names_sectors) |> longer_data())

  }


  #### 3.B EVIEWS SOLVER
  if(Rsolver == FALSE){
    eviews_model_solver(
      config_file = configuration,
      before_solving_data = data_for_solver,
      overwrite_eviews = path_eviews_exe)


    data_list_read <-purrr::set_names(scenario)  |> map(~read_3me_eviews_csv(file.path("data","temp","csv",str_c(.x,".csv") ) , variables_selection = variables_to_keep ))

    list_0 <- data_list_read[1]
    list_res <- data_list_read[-1] |> map(~select(.x,-baseline))

    data_list <- c(list_0,list_res) |>
      reduce(full_join, by = c("year", "variable")) |>
      aggregate_com_sec(bridge_com = bridge_commodities ,bridge_sec = bridge_sectors ,by_com = reagg_bool ,by_sec = reagg_bool , scenarios =  c("baseline",scenario |> unname())) |> purrr::compact() |>
      map(~ as.data.frame(.x) |> add_com_sec_names(commodities_names = names_commodities,sectors_names = names_sectors) |> longer_data())

  }

  ### PART 4 SAVING DATABASE
  save_3me_output(data_list[[1]], project_name, NULL,        formats = output_format)
  if("sec_com" %in% output_saved){
    save_3me_output(data_list[[4]], project_name, "sec_com", formats = output_format)}
  if("com" %in% output_saved){
    save_3me_output(data_list[[2]], project_name, "com",     formats = output_format)}
  if("sec" %in% output_saved){
    save_3me_output(data_list[[3]], project_name, "sec",     formats = output_format)}
  ## The values each parametric scenario ran with: <project_name>_params.csv
  write_3me_params(params_meta, project_name)



  return(data_list[[1]])


}
