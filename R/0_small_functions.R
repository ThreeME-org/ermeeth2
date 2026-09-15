## Small and useful functions

#' Title
#'
#' @param data A ThreeMe data.frame in long format.
#' @param test A regular expression to test for matching variables.
#' @param view If TRUE (default) results will be displayed in a separate window.
#'
#' @import dplyr utils
#' @return A data.frame containing the variables matching the test expression, as well as sectors and commodities corresponding to these variables where applicable.
#' @export
#'
variables_like <- function(data,test,view = TRUE){

  variable <- NULL
  sector <- NULL
  commodity <- NULL

  VariableShow <- data %>% dplyr::filter(grepl(test, variable)) %>% select(variable,sector,commodity) %>% unique()
  if(view == TRUE)
    {
      View(VariableShow)
    }else
    {
      VariableShow$variable
    }

}

# variables_like(data, "^EMS_CI")




#' Theoreme de pythagore
#' @description Cette fonction calcul l'hypothénuse d'un triangle rectangle
#'
#' @param a numeric (length 1), the first side of the triangle. Default 1
#' @param b numeric (length 1), the second side of the triangle. Default 1
#' @param fr_format afficher format francais
#'
#' @return character length 1 , hypothenuse
#' @export
#'
#' @importFrom stringr str_replace
#'
#' @examples
#' pythagore(a = 4, b = 3)
pythagore <- function(a = 1,b = 5, fr_format = FALSE){

c <- (a^2 + b^2)^(1/2)

if(fr_format == TRUE){c %>% as.character() %>% stringr::str_replace("\\.",",")}else{
  c %>% as.character()
}


}

# pythagore()

#' redirect
#' when you need to change the directed path because of markdowns
#'
#' @param normal_path normal path of the file
#' @param redirect new path of the file
#'
#' @return character string with the path changed
#' @export
#'
redirect <- function(normal_path, redirect = NULL){


  if(is.null(redirect)){
    res <- file.path(normal_path)
  }else{
    res <- file.path(paste0(redirect), normal_path)
  }

  res |>  normalizePath()
}

