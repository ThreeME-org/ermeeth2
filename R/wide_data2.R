#' Reshape long-format results back to wide format
#'
#' @description Pivots long-format results into one column per
#'   scenario-variable pair, returned either as a single data.frame or split
#'   into one data.frame per scenario.
#'
#' @param data data.frame in long format, with at least `year` and `variable`
#'   columns.
#' @param variables character vector. Variables to keep, or `NULL` for all.
#' @param out_format character. `"dataframe"` for a single wide data.frame,
#'   `"list"` for one data.frame per scenario.
#'
#' @returns A data.frame when `out_format` is `"dataframe"`, otherwise a named
#'   list of data.frames, one per scenario.
#' @export
wide_data_2<-function (data,
                       variables = NULL, out_format = "list")
{
  variable <- NULL
  scenario <- NULL
  values <- NULL

  # data= data_full
  # variables = c("GDP","SPEND_G_VAL")
  # out_format = "dataframe"

  if (prod(c("year", "variable") %in% names(data)) == 0) {
    stop(message = "The database is missing variables 'year' and/or 'variable'")
  }

  all_variables <- unique(data$variable)
  if (is.null(variables)) {
    variables <- all_variables
  } else {
    variables_not_found <- dplyr::setdiff(variables, all_variables)
    if (length(variables_not_found) > 0) {
      message(paste("Variable(s)", paste0(variables_not_found,
                                          collapse = ", "), "could not be found, they will be ignored."))
    }
    if (length(variables_not_found) == length(variables)) {
      stop(message = "Could not find any of the variables specified.")
    }
  }
  if (length(variables) > 1000) {
    message("There is a large number of variables... please wait... \n")
  }
  if (length(variables) >= 10000) {
    message("Maybe have a sip of coffee? \n")
  }
  if (length(variables) >= 12000) {
    message("Have you checked your emails or Slack messages? \n")
  }
  if (length(variables) >= 15000) {
    message("Maybe select less variables next time... \n")
  }
  if (is.null(out_format)) {
    out_format == "list"
  }
  if (!tolower(out_format) %in% c("list", "data.frame", "dataframe")) {
    message("out_format must be either 'list' or 'dataframe'. Reverting out_format to 'list'.\n")
    out_format <- "list"
  }else {
    out_format <- out_format %>% tolower() %>% stringr::str_remove_all("\\.")
  }
  data_short <- data %>% dplyr::filter(variable %in% variables)
  data_wide <- data_short %>% tidyr::pivot_wider(id_cols = c(year),
                                                 names_from = c(scenario,variable),
                                                 names_sep = ".",
                                                 values_from = values) %>% as.data.frame()

  scenarios <- data_short$scenario |> unique()

  if (out_format == "dataframe") {
    return(data_wide)
  }
  if (out_format == "list") {
    list_data <- purrr::set_names(scenarios)  |>
      purrr::map(~data_wide |>
                   dplyr::select(year, dplyr::starts_with(paste0(.x,"."))) |>
                   dplyr::rename_all(~str_remove(.x,"^.+\\.")))

    return(list_data)
  }
}
