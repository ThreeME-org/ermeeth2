#' Check the compiler settings after configuration
#'
#' @description Validates the advanced configuration arguments against what the
#'   compiler step actually produced, and reports any mismatch.
#'
#' @param base_advanced_arguments list. The advanced configuration block, as
#'   carried by the configuration object.
#'
#' @returns Called for its side effect of reporting check results to the
#'   console.
#' @export
post_compiler_checks <- function(base_advanced_arguments){
   list2env(base_advanced_arguments,envir = environment())
  ## 1.  Checking that the calib.csv file has all the endogenous variables declared in the model.prg file

  ### Get all equations from the equation list
  mod_from_dynamo <- readLines(file.path("src", "compiler", "model.prg")) |>
    str_remove_all("^a_3ME\\.append\\s")
  list_endo <- mod_from_dynamo |>

    ### Remove basic common things and keep LHS of equations
    str_remove_all("\\s+") |>
    str_remove_all("=.*$") |>

    ### remove funcions
    str_remove_all("\\w+\\(") |>
    str_replace_all("^|$","@") |>
    str_replace_all("(\\+|-|\\*|/|\\(|\\)|\\^)","@") |>

    ### remove numbers
    str_replace_all("@\\d+(\\.\\d+)?@","@") |>

    ### clean up
    str_replace_all("@+","@") |>

    ### remove extra variables
    str_replace_all("^(@\\w+\\@).*$","\\1") |>
    str_remove_all("@")

  equation_table <- data.frame(endogenous = list_endo, equation = mod_from_dynamo)

  calib_vars <- data.table::fread("src/compiler/calib.csv", data.table = FALSE ) |> names()

  missing_calibs <- setdiff(list_endo,calib_vars)

  if(length(missing_calibs) > 0){

    cli::cli_alert_danger("{length(missing_calibs)} endogenous variable{?s} {?is/are} not calibrated in {.file calib.csv}:")

    plop <- equation_table |> filter(endogenous %in% missing_calibs)
    cli_vector(plop$endogenous)
  }else{

    cli::cli_alert_success("All endogenous variables are present in {.file calib.csv}.")

  }

}

#' Locate EViews and settle the solver options
#'
#' @description Resolves the path to the EViews executable and reconciles the
#'   solver-related options, so the caller knows which solver will actually be
#'   used.
#'
#' @param config_list list. The configuration object, as returned by
#'   [readconfig()].
#'
#' @returns A list with elements `path_eviews_exe`, `Rsolver`, `rcpp_option`
#'   and `use.superlu`.
#' @export
eviews_checks<- function(config_list = configuration){

  # configuration <- NULL
  input <- NULL
  advanced_config <- NULL
  max_tresthor_capability <- NULL

  list2env(config_list,envir = environment())
  list2env(input,envir = environment())
  list2env(advanced_config,envir = environment())
  ## 2.  Check if EViews will be used
  if(file.size(file.path("src","compiler","model.prg")) > max_tresthor_capability *1000 & Rsolver == TRUE  ){
    Rsolver <- FALSE
    if (grepl("win", tolower(osVersion))){
      cli::cli_alert_warning("The model appears to be too large for the R solver; EViews will be used instead.")
    }else{
      cli::cli_abort(c("Cannot run this configuration of ThreeME on this computer.",
                       "x" = "The model appears to be too large for the R solver, and EViews requires Windows."))
    }
  }

  if( file.size(file.path("src", "compiler", "model.prg")) <= max_tresthor_capability * 1000 &
      grepl("win", tolower(osVersion)) == FALSE &
      Rsolver == FALSE
  ){
    cli::cli_alert_warning("EViews only runs on Windows. The model is small enough for the R solver, so it will be solved in R.")

    Rsolver <- TRUE
    rcpp_option = TRUE
    use.superlu = FALSE
  }


  ## 3. Checking and detecting automatically location of EViews.exe

  ### Detect location of EViews.exe
  if (grepl("win", tolower(osVersion)) & Rsolver == FALSE){

    if (file.exists(path_eviews_exe)) {
      cli::cli_alert_success("EViews found at {.path {path_eviews_exe}}.")
    } else {
      cli::cli_alert_warning("No EViews at the configured path {.path {path_eviews_exe}}; searching the usual locations.")

      nb_eviews.exe <- 0
      for (progfolder in c("C:/Program Files","C:/Program Files (x86)")){
        for (i in c(8:14)){
          # assign(paste0("eviews.exe.ver",i), paste0("C:/Program Files/EViews ",i,"/EViews",i,".exe"))
          path_eviews_exe_tested <- paste0(progfolder,"/EViews ",i,"/EViews",i,".exe")

          if (file.exists(path_eviews_exe_tested)) {
            path_eviews_exe <- path_eviews_exe_tested
            nb_eviews.exe <- nb_eviews.exe + 1
            cli::cli_alert_success("Possible path: {.path {path_eviews_exe}}")


          }

          path_eviews_exe_tested <- paste0(progfolder,"/EViews ",i,"/EViews",i,"_x64.exe")
          if (file.exists(path_eviews_exe_tested)) {
            path_eviews_exe <- path_eviews_exe_tested
            nb_eviews.exe <- nb_eviews.exe + 1
            cli::cli_alert_success("Possible path: {.path {path_eviews_exe}}")

          }
        }
      }
      if (nb_eviews.exe == 0) {
        cli::cli_abort(c("Cannot find EViews.",
                         "i" = "Check that EViews is installed if you wish to use the EViews solver."))

      }else{
        cli::cli_inform(c("i" = "{nb_eviews.exe} EViews path{?s} found. Using {.path {path_eviews_exe}}.",
                          " " = "To use another version, set {.code path_eviews_exe} in the config file."))

      }


      ### Define the path of Eviews default directory
      path_eviews_default <- paste0(getwd(), "/src/EViews/")
    }
    ### (if issue) Define the absolute path of Eviews default directory
    # path_eviews_default <- "C:/Users/reynesfgd/GitHub/ThreeME_V3/src/EViews/"
  }

  eviewspath <- ifelse(file.exists(path_eviews_exe),normalizePath(path_eviews_exe) , "")
  return <- list(
    path_eviews_exe =   eviewspath,
    Rsolver = Rsolver ,
    rcpp_option = rcpp_option ,
    use.superlu = use.superlu
  )

}
