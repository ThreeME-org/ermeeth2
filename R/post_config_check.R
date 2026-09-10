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
  list_endo <- model_prg_endogenous(mod_from_dynamo)

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

#' The endogenous variable of each equation of a compiled model
#'
#' @param equations the equations of `model.prg`, one per element.
#'
#' @returns a character vector as long as `equations`: the first variable on
#'   the left-hand side of each.
#' @keywords internal
model_prg_endogenous <- function(equations) {
  equations |>

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
}

#' Variables a scenario changes at or before the base year
#'
#' @description A scenario is expected to leave the calibration alone and to
#'   start moving the exogenous variables after the base year. These compare
#'   what a calibration script returns (`baseline_ch`, or one `shock_ch`) with
#'   the data it started from.
#'
#'   `base_year_changes()` looks at the base year, and at the exogenous
#'   variables only: the base year is the first period solved, so an
#'   endogenous variable changed there is recomputed anyway.
#'
#'   `lag_year_changes()` looks at the years before the base year, and at
#'   every variable: those years are never solved, they are only read as lags,
#'   so any change there, endogenous or not, moves the results.
#'
#' @param changed data frame returned by the calibration script, with a `year`
#'   column.
#' @param reference data frame it is compared with, with a `year` column:
#'   `calib.csv` for the baseline, the baseline calibration for a shock.
#' @param baseyear the base year.
#' @param endogenous names of the endogenous variables, which
#'   `base_year_changes()` does not check.
#' @param tolerance relative difference below which a value counts as
#'   unchanged.
#'
#' @returns a character vector: the variables whose value differs.
#' @keywords internal
base_year_changes <- function(changed, reference, baseyear,
                              endogenous = character(0), tolerance = 1e-10) {
  calib_changes(changed, reference, years = baseyear, skip = endogenous,
                tolerance = tolerance)
}

#' @rdname base_year_changes
lag_year_changes <- function(changed, reference, baseyear, tolerance = 1e-10) {
  years <- intersect(changed$year[changed$year < baseyear],
                     reference$year[reference$year < baseyear])
  calib_changes(changed, reference, years = years, tolerance = tolerance)
}

#' Variables whose value differs between two calibrations, over some years
#'
#' @param changed,reference data frames with a `year` column.
#' @param years the years to compare. A year missing from either is skipped.
#' @param skip variables not to compare.
#' @param tolerance relative difference below which a value counts as
#'   unchanged.
#'
#' @returns a character vector of variable names.
#' @keywords internal
calib_changes <- function(changed, reference, years, skip = character(0),
                          tolerance = 1e-10) {
  vars <- setdiff(intersect(names(changed), names(reference)), "year")
  vars <- vars[!tolower(vars) %in% tolower(skip)]
  years <- years[years %in% changed$year & years %in% reference$year]
  if (!length(vars) || !length(years)) return(character(0))

  as_matrix <- function(d) {
    d <- d[match(years, d$year), vars, drop = FALSE]
    matrix(suppressWarnings(as.numeric(unlist(d, use.names = FALSE))),
           nrow = length(years))
  }
  new <- as_matrix(changed)
  old <- as_matrix(reference)
  differs <- abs(new - old) > tolerance * pmax(1, abs(old))
  differs[is.na(differs)] <- xor(is.na(new), is.na(old))[is.na(differs)]
  vars[colSums(differs) > 0]
}

#' Warn about what a scenario changes at or before the base year
#'
#' @param variables the variables, as returned by [base_year_changes()] or
#'   [lag_year_changes()].
#' @param what the scenario, for the message.
#' @param baseyear the base year.
#' @param before `FALSE` for a change at the base year, `TRUE` for one in the
#'   years before it.
#'
#' @returns invisibly, `variables`.
#' @keywords internal
warn_base_year_changes <- function(variables, what, baseyear, before = FALSE) {
  if (!length(variables)) return(invisible(variables))
  if (before) {
    cli::cli_alert_warning("{what} changes {length(variables)} variable{?s} before the base year ({baseyear}). Those years are not solved, only read as lags, so this changes the results:")
  } else {
    cli::cli_alert_warning("{what} changes {length(variables)} exogenous variable{?s} at the base year ({baseyear}), which is the calibration:")
  }
  cli_vector(variables)
  invisible(variables)
}

#' Locate EViews and settle the solver options
#'
#' @description Resolves the path to the EViews executable and reconciles the
#'   solver-related options, so the caller knows which solver will actually be
#'   used. Asking for EViews anywhere but on Windows switches to the R solver,
#'   whatever the size of the model.
#'
#' @param config_list list. The configuration object, as returned by
#'   [readconfig()].
#'
#' @returns A list with elements `path_eviews_exe`, `Rsolver` and `Rcpp`.
#' @export
eviews_checks<- function(config_list = configuration){

  # configuration <- NULL
  input <- NULL
  advanced_config <- NULL
  Rcpp <- NULL

  list2env(config_list,envir = environment())
  list2env(input,envir = environment())
  list2env(advanced_config,envir = environment())
  ## 2.  Check if EViews will be used
  ##
  ## There is no cap on the size of the model any more: it was the limit of
  ## tresthor's dense solver, and neither of thortwo's backends has one.
  if (grepl("win", tolower(osVersion)) == FALSE & Rsolver == FALSE) {
    cli::cli_alert_warning("EViews only runs on Windows, so the model will be solved in R.")
    Rsolver <- TRUE
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
    Rcpp = Rcpp
  )

}
