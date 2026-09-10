
#' calibration table
#'
#' @param params_calib vector of the parameters from the ThreeME model with the code of the parameters used in the model (e.g.: ES_CHD)
#' @param project_name name of the project in which the values should be extracted
#' @param lang language chosen, two options "english" and "french"
#'
#' @return a gt object
#' @export
#' @import dplyr stringr gt
#'
#' @importFrom openxlsx read.xlsx
#'
calibration_table <- function(params_calib,
                              project_name,
                              lang = NULL)
{


  # By default setting on the language
  if(is.null(lang)){lang = "en"}

  ## Loading and filtering the data
  # Prendre à la place csv calib dans src/compiler/calib.csv
  data <- readRDS(file.path("data","output",paste0(project_name,".rds"))) |>
    dplyr::filter(year == startyear, scenario == "baseline") |>
    dplyr::select(-year, -scenario, - index_scen, - values_ref) |>
    dplyr::filter(stringr::str_detect(variable, "^ES")) |>
    dplyr::mutate(root = stringr::str_remove_all(variable, "(_C[A-Za-z0-9]{3})?(_S[A-Za-z0-9]{3})?$"),
                  com = stringr::str_extract(variable, "(C)[A-Za-z0-9]{3}$"),
                  sec = stringr::str_extract(variable, "(S)[A-Za-z0-9]{3}$" ))

  ## Dictionnary of labels for ThreeME parameters
  calib_labels <- openxlsx::read.xlsx(file.path("results", "quarto_templates", "calibration_parameters.xlsx")) |>
    dplyr::mutate(label = dplyr::case_when(lang == "fr" ~ label_fr,
                                           lang == "en" ~ label_en)) |>
    dplyr::select(Code, latex, label)


  data_table <- data |> dplyr::filter(root %in% toupper(params_calib)) |>
    merge(calib_labels, by.x = "root", by.y = "Code") |>
    dplyr::select(label, values, latex, commodity)

  # Producing the gt table
  gttable <- data_table |>
    dplyr::mutate(variable = stringr::str_c(label," ($$",latex,"$$)"),
                  produit = dplyr::case_when(is.na(commodity) & lang == "fr"~"tous",
                                             is.na(commodity) & lang == "en"~"all of them",
                                             !is.na(commodity)~commodity)) |>
    dplyr::select(variable, produit, values) |>
    gt::gt() |> gt::fmt_markdown()  |>
    gt::cols_label(variable = dplyr::case_when(lang == "fr"~"Param\u00e8tre",
                                               lang == "en"~"Parameter"),
                   produit = dplyr::case_when(lang == "fr"~"Produit",
                                              lang == "en"~"Product"),
                   values = dplyr::case_when(lang == "fr"~"Valeur",
                                             lang == "en"~"Value"))

  return(gttable)
}


