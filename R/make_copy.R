#' Make a copy of a variable
#'
#' @param var_name character string. Object from the envir to copy as a character string
#' @param append character string to append to the copy. Default is ".old"
#' @param envir Environment from where the variable is from. Default is GlobalEnv
#' @param envir2 Environment where to store the copy. Default is GlobalEnv.
#'
#' @returns object in the environment
#' @export
#'
#' @examples
#' \dontrun{
#' a <- 3
#' make_copy(a)
#'  }
make_copy <- function(var_name, append=".old", envir=.GlobalEnv, envir2 = .GlobalEnv) {
  assign(paste0(var_name, append), get(var_name, envir=envir), envir = envir2)
}
