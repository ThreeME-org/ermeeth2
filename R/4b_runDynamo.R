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
  dynamo_os <- "dynamo.exe"
  if(grepl("macOS",osVersion)){dynamo_os<-"dynamo"}
  if(grepl("Ubuntu",osVersion)){dynamo_os<-"dynamo"}

calib_files_tests <-file.exists(file.path(dynamo_path,calib))
model_files_tests <-file.exists(file.path(dynamo_path,model))

if(prod(calib_files_tests) == 0){
  cat("The following calib files were not found: \n ")
  cat( file.path(dynamo_path,calib)[which(calib_files_tests==FALSE)],sep = "\n")
  file.path(dynamo_path,calib)[which(calib_files_tests==FALSE)]
  cat("\n")
  Sys.sleep(3)
}

if(prod(model_files_tests) == 0){
  cat("The following model files were not found: \n ")
  cat( file.path(dynamo_path,model)[which(model_files_tests==FALSE)],sep = "\n")
  file.path(dynamo_path,model)[which(model_files_tests==FALSE)]
  cat("\n")
  Sys.sleep(3)
}
  ## check that dynamo_path has dynamo installed
 if(!file.exists(file.path(dynamo_path,"compiler","dynamo.exe"))){
   install_dynamo(dynamo_path)
 }

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
    calib %>% sapply(function(s) file.path(dynamo_path, s)),
    "",
    "# Model",
    model %>% sapply(function(s) file.path(dynamo_path, s))), f)
  close(f)

  sys::exec_wait(file.path(dynamo_path,"compiler",dynamo_os),
                 c(
                   file.path(dynamo_path, "compiler","dynamo.cfg"),
                   file.path(dynamo_path, "compiler","calib.csv"),
                   file.path(dynamo_path, "compiler","model.prg")
                 ),
                 std_out = TRUE, std_err = TRUE, timeout = 0)
}
