### run dynamo

#' Run the DynaMo compiler
#'
#' @description Runs DynaMo over the model files and checks that every expected
#'   output file was written, stopping with an error if any is missing.
#'
#' @param config_list list. The configuration object, as returned by
#'   [readconfig()].
#'
#' @returns Called for its side effects. Stops with an error if DynaMo did not
#'   produce all the expected files.
#' @export
run_dynamo <- function(config_list = configuration){

  # configuration <- NULL
  input <- NULL
  model_folder <- NULL
  baseyear <- NULL
  advanced_config <- NULL
  iso3 <- NULL
  lastyear <- NULL
  max_lags <- NULL

  list2env(config_list,envir = environment())
  list2env(input,envir = environment())

  dynamo_files_out <- c("calib.csv", "model.prg")

  lists_files <- file.path("model", model_folder, lists_files)
  calib_files <- file.path("model", model_folder, calib_files)
  model_files <- file.path("model", model_folder, model_files)


  readLines(file.path("src", last(lists_files))) |>
    str_replace_all("(^\\s*%baseyear\\s*:=\\s*).*$", str_c("\\1", baseyear)) |>
    # str_replace_all("(^include\\s+\\.\\\\R_lists).*$", str_c("\\1_", iso3)) |>
    writeLines(file.path("src", last(lists_files)))

  ## A if no compiler needed
  if(advanced_config$skip_compiler == TRUE & advanced_config$recompile_model == TRUE){
    if(sum(file.exists(file.path("src","compiler", dynamo_files_out)) ) == length(dynamo_files_out) ){
      cli::cli_alert_success("Found pre-existing DynaMo output files.")

    }else{
      cli::cli_abort(c("Compiler files not found.",
                       "x" = "Some of the compiler output files are missing from {.path src/compiler}.",
                       "i" = "Add them manually, or set {.code skip_compiler = FALSE} in the config to generate them."))
    }

  }else{

    ## B With a compiler

    ### B1. Remove existing dynamo output

    if(sum(file.exists(file.path("src","compiler",dynamo_files_out)) ) >0 ){
      cli::cli_alert("Found pre-existing DynaMo output files, deleting them.")
    }

    purrr::quietly(map)(dynamo_files_out ,
                 function(file = .x){
                   if(file.exists(file.path("src","compiler",file) ) ){
                     file.remove(file.path("src","compiler",file))
                   }
                 }
    )$result

    ### B2. Run the DynaMo compiler

    runDynaMo(iso3, baseyear, lastyear, calib_files, model_files, max_lags)
  }

  ### B3. Bug correction if dynamo doesn't put the files in the right place
  purrr::quietly(map)(dynamo_files_out,
               function(file = .x){
                 if(file.exists(file)){
                   file.copy(
                     from=file,
                     to = file.path("src","compiler",file),
                     overwrite = TRUE)
                   file.remove(file)
                 }
               }
  )$result


  ## C. Check if compiled successfully
  missing_files <- file.path("src","compiler",dynamo_files_out)[file.exists(file.path("src","compiler",dynamo_files_out)) == FALSE ] |> basename()

  if(length(missing_files) == 0){

    cli::cli_alert_success("DynaMo succeeded, continuing...")

  }else{

    cli::cli_abort(c("DynaMo error.",
                     "x" = "DynaMo could not write {.file {missing_files}}."))
  }
}
