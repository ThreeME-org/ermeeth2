# Convert database into long format which is easier to be manipulated
#' Reshape simulation output to long format
#'
#' @description Pivots wide simulation output into the long format the plot and
#'   table functions expect, adding the reference-scenario columns
#'   `values_ref` and `index_scen`.
#'
#' @param data data.frame. Simulation output.
#' @param nom_ref character. Name of the reference (baseline) scenario.
#'
#' @returns A data.frame with columns `year`, `variable`, `scenario`, `values`,
#'   `values_ref` and `index_scen`.
#' @export
longer_data <- function(data,nom_ref = "baseline"){

  variable <- NULL
  sector <- NULL
  commodity <- NULL
  values_ref <- NULL

  data_long <- data %>%
    as.data.frame() %>%
    mutate(values_ref = get(nom_ref)) %>%
    pivot_longer(c(-year, -variable, -sector, -commodity, -values_ref),
                 names_to = "scenario", values_to = "values") %>%
    mutate(index_scen = case_when(
      scenario == nom_ref ~ 1,
      scenario != nom_ref ~ 0)) %>%
    as.data.frame()

  return(data_long)
}


#' Reshape simulation output to long format with sector and commodity names
#'
#' @description Like [longer_data()], but additionally splits the sector and
#'   commodity suffixes out of the variable names and replaces the codes with
#'   readable labels.
#'
#' @param data data.frame. Simulation output.
#' @param sector_names_table data.frame with `code` and `name` columns mapping
#'   sector codes to labels, or `NULL` to leave the codes untouched.
#' @param commodity_names_table data.frame with `code` and `name` columns
#'   mapping commodity codes to labels, or `NULL` to leave the codes untouched.
#' @param scenario_ref character. Name of the reference (baseline) scenario.
#'
#' @returns A long-format data.frame, with `sector` and `commodity` columns in
#'   addition to those produced by [longer_data()].
#' @export
long_data_2 <- function (data,
                         sector_names_table = names_sectors,
                         commodity_names_table = names_commodities,
                         scenario_ref = "baseline")
{

  # names_sectors <- NULL
  # names_commodities <- NULL
  variable <- NULL
  sector <- NULL
  commodity <- NULL


  if (inherits(data, "data.frame")) {
    scenarios <- str_extract(names(data), "^.+\\.") %>% str_remove("\\.") %>%
      unique() %>% stats::na.omit()
    if (length(scenarios) == 0) {
      stop(message = "Could not find scenario names. Make sure you use a wide ThreeMe data.frame with scenario names attached to the variables i.e. 'baseline.GDP'.")
    }
    data_list <- purrr::set_names(scenarios) %>% purrr::map(~data %>%
                                                              dplyr::select(year, dplyr::starts_with(paste0(.x,
                                                                                                            "."))) %>% dplyr::rename_all(~str_remove(.x,
                                                                                                                                                     "^.+\\.")))
  } else {
    if (inherits(data, "list")) {
      data_list <- data
      scenarios <- names(data_list)
    }
    else {
      stop(message = "data must be a ThreeMe data.frame or a ThreeMe data list created by wide.data.")
    }
  }
  long_data <- data_list %>% purrr::imap(~.x %>% tidyr::pivot_longer(cols = !year,
                                                                     names_to = "variable") %>% rename(`:=`(!!.y, value))) %>%
    purrr::reduce(full_join, by = c("year", "variable"))|>
    mutate(sector = as.character(NA), commodity = as.character(NA)) %>%
    mutate(sector = ifelse(grepl("_S[a-zA-Z0-9]{3}$", variable),
                           str_replace_all(variable, "^.+_(S[a-zA-Z0-9]{3})$",
                                           "\\1"), sector), commodity = ifelse(grepl("_C[a-zA-Z0-9]{3}(_S[a-zA-Z0-9]{3})?$",
                                                                                     variable), str_replace_all(variable, "^.+_(C[a-zA-Z0-9]{3})(_S[a-zA-Z0-9]{3})?$",
                                                                                                                "\\1"), commodity), commodity = ifelse(commodity ==
                                                                                                                                                         "CONS", as.character(NA), commodity)) %>% arrange(variable)
  if (!is.null(sector_names_table)) {
    long_data <- long_data %>% mutate(sector = stringr::str_replace_all(toupper(sector),
                                                                        purrr::set_names(sector_names_table$name, paste0("^",
                                                                                                                         toupper(sector_names_table$code), "$"))))
  }
  if (!is.null(commodity_names_table)) {
    long_data <- long_data %>% mutate(commodity = stringr::str_replace_all(toupper(commodity),
                                                                           purrr::set_names(commodity_names_table$name, paste0("^",
                                                                                                                               toupper(commodity_names_table$code), "$"))))
  }
  return(long_data  %>% longer_data(nom_ref = scenario_ref)  %>% as.data.frame())
}
