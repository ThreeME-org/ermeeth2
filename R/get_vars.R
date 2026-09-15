#' get all elements from a vector with multiple regexes
#'
#' @param regex_list character vector of regexes to look for in a vector
#' @param vector character vector where to look for
#' @param negate boolean, set to TRUE to extract elements that are DO NOT verify the regexes. Default : FALSE
#' @param case_sensitive boolean, set to FALSE to ignore the case from your search. Default : FALSE
#' @param results_as_list boolean, set to TRUE if looking for results in list form, where every element is a character vector of elements from the search vector verifying (or not if negate = FALSE) the regexes.
#'
#' @returns character vector compiled
#' @importFrom purrr map reduce
#'
#' @export
#'
#' @examples get_regex(regex_list = c("y$"), vector = month.name)
#'
get_regex <- function(regex_list = c("*"),vector=c(""), negate = FALSE,case_sensitive = FALSE , results_as_list = FALSE){
  # regex_list = c("gdp","Y.*")
  # vector = list_of_variables
  # negate = FALSE
  # case_sensitive = FALSE

  if(!is.character(regex_list)){stop(message("argument regex_list must be a character vector.\n")) }
  if(!is.character(vector)){stop(message("argument vector must be a character vector.\n")) }

  vec <- unique(vector)
if(results_as_list){

  res <- regex_list |> purrr::map( ~vec[grepl(.x,vec,ignore.case = !case_sensitive)== !negate] ) |>
    purrr::reduce(c) |> unique()

}else{
  res <- regex_list |> purrr::map( ~vec[grepl(.x,vec,ignore.case = !case_sensitive)== !negate] )
}
  return(res)
}
