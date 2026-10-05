## Dynamo Run

#' Run Dynamo Compiler for models
#'
#' @param iso3 country code extension
#' @param baseyear baseyear of simulations
#' @param lastyear last year of simulations
#' @param calib vector list of calibration files
#' @param model vector list of model files
#' @param max_lags number of years before baseyear to generate based on lags used the equations of the model
#' @param dynamo_path path where dynamo is with the calib files and model files
#'
#' @return created the model.prg and calib.csv files
#' @importFrom sys exec_wait
#'
#' @export
#'
runDynaMo <- function(iso3, baseyear, lastyear, calib, model, max_lags = 3,
                      dynamo_path = file.path("src")) {
  # One binary per OS. Linux has its own: handing it the macOS `dynamo` fails
  # with `Syntax error: "(" unexpected`, the shell trying to read it as a script.
  dynamo_os <- switch(Sys.info()[["sysname"]],
                      Windows = "dynamo.exe",
                      Darwin  = "dynamo",
                      "dynamo_ubuntu")

  # The config may not spell the file names with the case they have on disk.
  # macOS and Windows do not care, Linux does.
  calib_paths <- match_file_case(file.path(dynamo_path, calib))
  model_paths <- match_file_case(file.path(dynamo_path, model))

  calib_files_tests <- file.exists(calib_paths)
  model_files_tests <- file.exists(model_paths)

  if(prod(calib_files_tests) == 0){
    cli::cli_alert_warning("The following calib files were not found:")
    cli_vector(calib_paths[which(calib_files_tests==FALSE)])
    Sys.sleep(3)
  }

  if(prod(model_files_tests) == 0){
    cli::cli_alert_warning("The following model files were not found:")
    cli_vector(model_paths[which(model_files_tests==FALSE)])
    Sys.sleep(3)
  }

  ## check that dynamo_path has dynamo installed
  if(!file.exists(file.path(dynamo_path,"compiler","dynamo.exe"))){
    install_dynamo(dynamo_path)
  }

  ## the binary of this OS may be missing from an existing install
  dynamo_bin <- file.path(dynamo_path, "compiler", dynamo_os)
  if(!file.exists(dynamo_bin)){
    utils::unzip(system.file("dynamo_inst.zip", package = "ermeeth2"),
                 files = file.path("dynamo", dynamo_os, fsep = "/"),
                 exdir = file.path(dynamo_path, "compiler"), junkpaths = TRUE)
  }
  # unzip() drops the executable bit
  if(dynamo_os != "dynamo.exe"){Sys.chmod(dynamo_bin, "755")}

  f <- file(file.path(dynamo_path,"compiler","dynamo.cfg"))


  writeLines(c(
    "# CountryCalib",
    file.path(dynamo_path, "data","shadowfile_readme.mdl"),
    "",
    "# ListsParameters",
    "",
    "# MaxLags",
    max_lags, "",
    "# Baseyear",
    baseyear, "",
    "# Lastyear",
    lastyear,
    "",
    "# Calib",
    calib_paths,
    "",
    "# Model",
    model_paths), f)
  close(f)

  status <- sys::exec_wait(dynamo_bin,
                 c(
                   file.path(dynamo_path, "compiler","dynamo.cfg"),
                   file.path(dynamo_path, "compiler","calib.csv"),
                   file.path(dynamo_path, "compiler","model.prg")
                 ),
                 std_out = TRUE, std_err = TRUE, timeout = 0)

  # The Linux build ignores the name it is given for the calibration and
  # writes `datamancer.csv` instead.
  calib_out <- file.path(dynamo_path, "compiler", "calib.csv")
  stray <- c("datamancer.csv", file.path(dynamo_path, "compiler", "datamancer.csv"))
  stray <- stray[file.exists(stray)]
  if(!file.exists(calib_out) && length(stray) > 0){
    file.copy(stray[1], calib_out, overwrite = TRUE)
    file.remove(stray[1])
    cli::cli_alert_info("DynaMo wrote the calibration to {.file {stray[1]}}, moved to {.file {calib_out}}.")
  }

  status
}


# Give each path the case its file has on disk, when the name only differs by
# case and the match is unambiguous. Paths that exist as written, or that match
# nothing, are returned unchanged.
match_file_case <- function(paths) {
  vapply(paths, function(path) {
    if (file.exists(path)) return(path)
    on_disk <- list.files(dirname(path), all.files = TRUE)
    hit <- on_disk[tolower(on_disk) == tolower(basename(path))]
    if (length(hit) == 1) file.path(dirname(path), hit) else path
  }, character(1), USE.NAMES = FALSE)
}
