#' Solve the model with EViews
#'
#' @description Writes the calibration csv files, hands the model over to
#'   EViews for solving, reads the results back, then cleans up the temporary
#'   calibration files. The solve options of the configuration
#'   (`eviews_algorithm`, `eviews_digits`, `eviews_max_iter`) are written into
#'   `src/EViews/solve.prg` first, by [eviews_solve_options()].
#'
#'   With `recompile_model_eviews = FALSE` in the configuration, EViews
#'   reopens the workfile saved by an earlier run instead of loading the
#'   model, the calibration and the baseline again. The option is followed as
#'   given: whether that workfile exists and holds the current model is not
#'   checked, and cannot be.
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
  recompile_model_eviews <- NULL
  save_files_res <- NULL
  # . <- NULL
  scenario <- NULL
  eviews_timeout <- NULL
  eviews_algorithm <- NULL
  eviews_digits <- NULL
  eviews_max_iter <- NULL

  list2env(config_file,envir = environment())
  list2env(config_file$input,envir = environment())
  list2env(config_file$input$advanced_config,envir = environment())
  path_eviews_exe_2 = overwrite_eviews

  ## Whether EViews loads the model, the calibration and the baseline afresh,
  ## or reopens the workfile an earlier run saved. The option is followed as
  ## given: nothing here can tell whether a workfile found under the expected
  ## name holds the current model, so there is no check to base a decision on.
  recompile_model <- !isFALSE(recompile_model_eviews)

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

  # Write the solve options into solve.prg, in the branch configuration.prg selects
  solveopt <- stringr::str_match(readLines("src/EViews/configuration.prg", warn = FALSE),
                                 "^\\s*%solveopt\\s*=\\s*\"([^\"]*)\"")[, 2]
  solveopt <- solveopt[!is.na(solveopt)]
  eviews_solve_options(algorithm = eviews_algorithm,
                       digits    = eviews_digits,
                       max_iter  = eviews_max_iter,
                       solveopt  = if (length(solveopt)) solveopt[[1]] else "u0")

  if (recompile_model){


    # Splits calib.csv in n files: to get around Eviews limitation regarding loading big csv files
    cli::cli_alert_info("Loading {.file calib.csv} ({round(file.size(\"src/compiler/calib.csv\")/1000000,3)} MB)")

    calib <- fread("src/compiler/calib.csv", data.table = FALSE) |>
      select(-baseyear) |>
      mutate(year = year + baseyear)

    ncol.in.calib <-  ncol(calib) |> as.numeric()
    ncol.in.splitcalib <- ncol.in.calib/nb_calib_files

    if (nb_calib_files > 1) {

      cli::cli_alert_warning("{.file calib.csv} is larger than {limit.size.calib.csv} MB (the EViews limit when importing csv files). Splitting it into {nb_calib_files} files of about {round(ncol.in.splitcalib)} variables each.")

      for (i in  c(1:nb_calib_files)){

        range.col <- (1+(i-1)*floor(ncol.in.splitcalib)):(i*(ifelse(i!=nb_calib_files,floor(ncol.in.splitcalib),ncol.in.splitcalib)))

        assign(paste0("calib",i), calib |> select(all_of(range.col)))

        cli::cli_alert("Saving {.file calib{i}.csv}")

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
    cli::cli_alert_info("Run ThreeME in EViews: scenario {.val {scen}} ({shock_nb}/{length(scenario)})")
    sys::exec_wait(normalizePath(path_eviews_exe_2), c(stringr::str_c(eviews_default_path, "run_main_from_R.prg")), timeout = as.numeric(eviews_timeout))

    shock_nb = shock_nb + 1
  }


  # Removing calib1 and calib 2 files
  for (i in 1:nb_calib_files) {
    file <- stringr::str_c("src/compiler/calib",i,".csv")
    if (file.exists(file)) {
      cli::cli_alert("Removing file {.file calib{i}.csv}")
      file.remove(file)
    }
  }




}

#' The solution algorithms EViews offers
#'
#' @returns a character vector of the names used in the configuration
#'   (`eviews_algorithm`), named by the label the addin shows.
#' @keywords internal
eviews_algorithms <- function() {
  c("Broyden" = "broyden", "Newton" = "newton", "Gauss-Seidel" = "gauss-seidel")
}

#' Set the EViews solve options in `solve.prg`
#'
#' @description Rewrites three arguments of the `solve()` call in
#'   `src/EViews/solve.prg`: `o` (the algorithm), `g` (the number of digits the
#'   solution is rounded to) and `m` (the maximum number of iterations). The
#'   other arguments, the comments and the rest of the file are left alone, and
#'   the file is not written when nothing changes.
#'
#'   `solve.prg` holds one `solve()` call per value of `%solveopt`. Only the
#'   one under `if %solveopt = "<solveopt>"` is edited; [eviews_model_solver()]
#'   passes the value `src/EViews/configuration.prg` sets.
#'
#' @param algorithm the solution algorithm: `"broyden"`, `"newton"` or
#'   `"gauss-seidel"`.
#' @param digits number of digits to round the solution to.
#' @param max_iter maximum number of iterations, at most 100000.
#' @param file path to `solve.prg`.
#' @param solveopt the `%solveopt` branch whose `solve()` call is edited.
#'
#' @returns invisibly, the `solve()` call as it now reads.
#' @export
#'
#' @examples
#' \dontrun{
#' eviews_solve_options(algorithm = "newton", digits = 8, max_iter = 10000)
#' }
eviews_solve_options <- function(algorithm = "broyden",
                                 digits = 10,
                                 max_iter = 5500,
                                 file = file.path("src", "EViews", "solve.prg"),
                                 solveopt = "u0") {

  algorithm <- match.arg(tolower(algorithm), eviews_algorithms())
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 1 || digits != round(digits)) {
    cli::cli_abort("{.arg digits} must be a whole number, at least 1.")
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1L || is.na(max_iter) ||
      max_iter < 1 || max_iter > 100000 || max_iter != round(max_iter)) {
    cli::cli_abort("{.arg max_iter} must be a whole number between 1 and 100000.")
  }
  if (!file.exists(file)) cli::cli_abort("no such file: {.file {file}}")

  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")

  branch <- grep(paste0("^\\s*if\\s+%solveopt\\s*=\\s*\"", solveopt, "\""),
                 lines, ignore.case = TRUE)
  calls <- grep("\\.solve\\(", lines)
  target <- if (length(branch)) calls[calls > branch[1L]][1L] else NA_integer_
  if (is.na(target)) {
    cli::cli_abort(c("Cannot find the {.code solve()} call for {.code %solveopt = \"{solveopt}\"} in {.file {file}}.",
                     "i" = "The EViews solve options were not applied."))
  }

  ## The call ends at its closing bracket; what follows is an EViews comment
  ## that describes the options, and must not be edited.
  line <- lines[target]
  end  <- regexpr(")", line, fixed = TRUE)
  call <- substr(line, 1L, end)
  rest <- substr(line, end + 1L, nchar(line))

  set_arg <- function(call, key, value) {
    pattern <- paste0("([(,]\\s*", key, "\\s*=\\s*)[^,)]*")
    if (grepl(pattern, call)) {
      sub(pattern, paste0("\\1", value), call)
    } else {
      sub(".solve(", paste0(".solve(", key, "=", value, ", "), call, fixed = TRUE)
    }
  }
  code <- c(broyden = "b", newton = "n", "gauss-seidel" = "g")[[algorithm]]
  call <- call |>
    set_arg("m", format(max_iter, scientific = FALSE)) |>
    set_arg("g", format(digits, scientific = FALSE)) |>
    set_arg("o", code)

  new_line <- paste0(call, rest)
  if (!identical(new_line, line)) {
    lines[target] <- new_line
    writeLines(lines, file, useBytes = TRUE)
  }
  invisible(trimws(call))
}
