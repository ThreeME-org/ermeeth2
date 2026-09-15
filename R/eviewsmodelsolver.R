#' Call eviews solver
#'
#' @param config_file Ermeeth2 configuration list
#' @param before_solving_data database before data is solved
#' @param overwrite_eviews path to eviews.exe
#' @param limit_size_calib_csv
#'
#' import stringr
#'


#' Solve the model with EViews
#'
#' @description Writes the calibration csv files, hands the model over to
#'   EViews for solving, reads the results back, then cleans up the temporary
#'   calibration files.
#'
#' @param config_file list. The configuration object, as returned by
#'   [readconfig()].
#' @param before_solving_data data to pass to the solver before solving.
#' @param overwrite_eviews logical. Whether to overwrite the existing EViews
#'   workfile.
#' @param limit_size_calib_csv numeric. Maximum size, in the units used by the
#'   EViews workflow, of a single calibration csv before it is split.
#'
#' @returns Called for its side effects on the EViews workflow and the files in
#'   `src/compiler`.
#' @export
eviews_model_solver<- function(config_file = configuration,
                               before_solving_data = NULL,
                               overwrite_eviews = NULL,
                               limit_size_calib_csv = 12){

  # configuration <- NULL
  iso3 <- NULL
  firstyear <- NULL
  baseyear <- NULL
  tolerance_calib_check <- NULL
  lastyear <- NULL
  recompile_model <- NULL
  save_files_res <- NULL
  # . <- NULL
  scenario <- NULL
  eviews_timeout <- NULL

  list2env(config_file,envir = environment())
  list2env(config_file$input,envir = environment())
  list2env(config_file$input$advanced_config,envir = environment())
  path_eviews_exe_2 = overwrite_eviews

  compil = "dynamo"
  data_for_solver_2 <- before_solving_data

  ## Run ThreeMe
  ## 1 Edit eviews run file
  # Define the path of Eviews default directory
  eviews_default_path <- stringr::str_c(getwd(), "/src/EViews/")

  limit.size.calib.csv <- limit_size_calib_csv
  nb_calib_files <- ceiling(file.size("src/compiler/calib.csv")/1000000/limit.size.calib.csv)

  # Rewrite run_main_from_R.prg
  readLines("src/EViews/run_main_from_R.prg") |>
    stringr::str_replace_all("(^%iso3\\s*=\\s*).*$", stringr::str_c("\\1\\\"", iso3, "\\\"")) |>
    stringr::str_replace_all("(^%path_eviews_default\\s*=\\s*).*$", stringr::str_c("\\1\\\"", eviews_default_path,"\\\"")) |>

    stringr::str_replace_all("(^%warning\\s*=\\s*).*$", stringr::str_c("\\1\\\"", warning, "\\\"")) |>
    stringr::str_replace_all("(^%compil\\s*=\\s*).*$",replacement =paste0("\\1\\\"", compil,"\\\"")) |>

    stringr::str_replace_all("(^%firstyear\\s*=\\s*).*$", stringr::str_c("\\1\\\"", firstyear, "\\\"")) |>
    stringr::str_replace_all("(^%baseyear\\s*=\\s*).*$", stringr::str_c("\\1\\\"", baseyear, "\\\"")) |>
    stringr::str_replace_all("(^%tolerance_calib_check\\s*=\\s*).*$", stringr::str_c("\\1\\\"", tolerance_calib_check, "\\\"")) |>
    stringr::str_replace_all("(^%nb_calib_files\\s*=\\s*).*$", stringr::str_c("\\1\\\"", nb_calib_files, "\\\"")) |>

    stringr::str_replace_all("(^%lastyear\\s*=\\s*).*$", stringr::str_c("\\1\\\"", lastyear, "\\\"")) |>
    stringr::str_replace_all("(^%recompile_model\\s*=\\s*).*$", stringr::str_c("\\1\\\"", recompile_model, "\\\"")) |>

    stringr::str_replace_all("(^%save_files_res\\s*=\\s*).*$", stringr::str_c("\\1\\\"", save_files_res, "\\\"")) |>

    writeLines("src/EViews/run_main_from_R.prg")

  if (recompile_model){


    # Splits calib.csv in n files: to get around Eviews limitation regarding loading big csv files
    cat(stringr::str_c("Loading calib.csv file. Size: ", round(file.size("src/compiler/calib.csv")/1000000,3), " MB\n"))

    calib <- fread("src/compiler/calib.csv", data.table = FALSE) |>
      select(-baseyear) |>
      mutate(year = year + baseyear)

    ncol.in.calib <-  ncol(calib) |> as.numeric()
    ncol.in.splitcalib <- ncol.in.calib/nb_calib_files

    if (nb_calib_files > 1) {

      (paste("calib.csv is larger than", limit.size.calib.csv, "MB (EViews limit when importing csv files). Splitting csv into", nb_calib_files,"files. (Each file contains on average", ncol.in.splitcalib,"variables):")) |> message_warning()

      for (i in  c(1:nb_calib_files)){

        range.col <- (1+(i-1)*floor(ncol.in.splitcalib)):(i*(ifelse(i!=nb_calib_files,floor(ncol.in.splitcalib),ncol.in.splitcalib)))

        assign(paste0("calib",i), calib |> select(.,all_of(range.col)))

        (paste0("Saving calib",i,".csv")) |> message_save()

        write.csv(get(paste0("calib",i)),file.path("src","compiler",paste0("calib",i,".csv")))

      }


    }



  }
  # Solving the model

  solved_data <- data_for_solver_2

  ## stock the results in solved_data
  shock_nb = 1
  for (scen in scenario){

    readLines("src/EViews/run_main_from_R.prg") |>
      stringr::str_replace_all("(^%scenario\\s*=\\s*).*$", stringr::str_c("\\1\\\"",scen,"\\\"")) |>

      writeLines("src/EViews/run_main_from_R.prg")

    if(shock_nb>1){
      readLines("src/EViews/run_main_from_R.prg") |>
        stringr::str_replace_all("(^%warning\\s*=\\s*).*$", stringr::str_c("\\1\\\"", "FALSE", "\\\"")) |>
        stringr::str_replace_all("(^%recompile_model\\s*=\\s*).*$", stringr::str_c("\\1\\\"", "FALSE", "\\\"")) |>

        writeLines("src/EViews/run_main_from_R.prg")

    }


    # Run ThreeME in Eviews
    cat(stringr::str_c("Run ThreeME in Eviews: scenario ",scen," (",shock_nb,"/",length(scenario),")","\n"))
    sys::exec_wait(normalizePath(path_eviews_exe_2), c(stringr::str_c(eviews_default_path, "run_main_from_R.prg")), timeout = eviews_timeout)

    shock_nb = shock_nb + 1
  }


  # Removing calib1 and calib 2 files
  for (i in 1:nb_calib_files) {
    file <- stringr::str_c("src/compiler/calib",i,".csv")
    if (file.exists(file)) {
      cat(stringr::str_c("Removing file calib",i,".csv\n"))
      file.remove(file)
    }
  }




}
