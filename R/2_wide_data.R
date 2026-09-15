## wide_data_3me


# data <- readRDS("tests/oilprice_fra_c28_s32.rds")
# scenario_to_analyse <- "oilprice_fra"
# classification <- "c28_s32"
# variables <- NULL
# variables <- c("GDP","WAGES","MERIEM")
# out_format = "list"
# scenarios = c("baseline",scenario_to_analyse)
# all_variables <- unique(data$variable)
# my_variables<- sample(all_variables, size = 10000)
#
# plop <- wide_data(data = data,
#                   scenarios = scenarios,
#                   variables = variables,
#                   out_format = "list")

#
# ploplist <- wide_data(data = data, variables = variables,out_format = "list")
# ploplist2 <- wide_data(data = data, variables = variables,out_format = "list")
#
# coountry_scenar <- list(FR= ploplist,DE = ploplist2)
# new <- ploplist %>% map(~.x %>%  mutate(ratio = GDP/WAGES))


# variables <- c("SALAIRE")

#' Transform a long ThreeMe database to wide format
#'
#' @param data data.frame: A ThreeMe dataframe (long format)
#' @param scenarios character vector: the scenarios that should be selected, by default uses baseline and scenario_to_analyse
#' @param variables character vector: the variables to be selected. NULL by default, takes all the variables in the database
#' @param out_format output format, either 'dataframe' or 'list'. By default 'dataframe' returns a dataframe with the name of scenario preceding variable name
#'
#' @return a data.frame or a list
#' @export
#'
#' @importFrom stringr str_remove_all str_remove
#' @importFrom purrr map set_names
#' @importFrom tidyr pivot_wider
#' @import dplyr
#'
wide_data <- function(data ,
                      scenarios = c("baseline",scenario_to_analyse) ,
                      variables = NULL ,
                      out_format = "dataframe"
                      ){


  variable <- NULL


  ## scenario argument check
  if(is.null(scenarios)){
    scenarios <- "baseline"
  }

  ## database threeme check
  if( prod(c("year","variable") %in% names(data)) == 0 ){
    stop(message = "The database is missing variables 'year' and/or 'variable'")
  }
  if( prod(scenarios %in% names(data)) == 0 ){
    missing_scen <- dplyr::setdiff(scenarios,names(data))
    stop(message = paste("The database is missing scenario variable(s):", missing_scen))
  }

  ## variables check
  all_variables <- unique(data$variable)

  if(is.null(variables)){
    variables <- all_variables
  }else{
    variables_not_found <- dplyr::setdiff(variables,all_variables)
    if (length(variables_not_found)>0 ){
      message(paste("Variable(s)", paste0(variables_not_found,collapse = ", "), "could not be found, they will be ignored."))
    }
    if(length(variables_not_found) == length(variables)){
      stop(message="Could not find any of the variables specified.")
    }

  }

  if(length(variables) > 1000){
    message("There is a large number of variables... please wait... \n")
  }

  if(length(variables) >= 10000){
    message("Maybe have a sip of coffee? \n")
  }

  if(length(variables) >= 12000){
    message("Have you checked your emails or Slack messages? \n")
  }

  if(length(variables) >= 15000){
    message("Maybe select less variables next time... \n")
  }

  ## checking out_format
  if(is.null(out_format)){out_format == "list"}
  if(!tolower(out_format) %in% c("list","data.frame","dataframe")){
   message("out_format must be either 'list' or 'dataframe'. Reverting out_format to 'list'.\n")
    out_format <- "list"
  }else{
    out_format  <-  out_format %>% tolower() %>% stringr::str_remove_all("\\.")
  }

  ## Filtering the data
  data_short <- data %>% dplyr::filter(variable %in% variables)


  ## Making it wide
  data_wide <- data_short %>%
    tidyr::pivot_wider(
      id_cols = year ,
      names_from = variable,
      names_sep = "." ,
      values_from = dplyr::all_of(scenarios) )%>% as.data.frame()

  if(out_format == "dataframe"){
    return(data_wide)
  }

  if(out_format == "list"){
    list_data <- purrr::set_names(scenarios)  %>%
      purrr::map(~data_wide %>%
                   dplyr::select(year, dplyr::starts_with(paste0(.x,"."))) %>%
                   dplyr::rename_all(~str_remove(.x,"^.+\\.")) ## should change to use scenario.name
                 )
      return(list_data)


  }


}




