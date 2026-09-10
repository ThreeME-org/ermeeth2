### Install Dynamo

#' Install Dynamo
#' @description  This function installs the Dynamo executable in a specified location
#' @param dynamo_path directory path where to install Dynamo
#' @param overwrite TRUE to overwrite existing Dynamo install files
#'
#' @return Dynamo installation in the specified location
#' @export
#'
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#'  install_dynamo("../src/compiler")
#' }
install_dynamo <- function(dynamo_path = getwd() , overwrite = TRUE){


  #1. copy the dynamo files
  utils::unzip(system.file("dynamo_inst.zip", package = "ermeeth2"),exdir =file.path(dynamo_path,"compiler"),overwrite = overwrite)

  #2 Build other structural files
  if(!dir.exists(file.path(dynamo_path,"data"))){dir.create(file.path(dynamo_path,"data"))}

  if(!dir.exists(file.path(dynamo_path,"model"))){dir.create(file.path(dynamo_path,"model"))}

  #3 Move the shadowfile (the archive nests everything under a dynamo/ folder)
  shadowfile <- list.files(file.path(dynamo_path, "compiler"),
                           pattern = "^shadowfile_readme\\.mdl$",
                           recursive = TRUE, full.names = TRUE)

  if (length(shadowfile) > 0) {
    file.copy(from = shadowfile[1],
              to = file.path(dynamo_path, "data", "shadowfile_readme.mdl"),
              overwrite = TRUE)
    file.remove(shadowfile[1])
  }

}

