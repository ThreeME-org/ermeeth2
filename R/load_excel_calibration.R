## Loading a specifically formatted excel file for threeme scenarios

#'  Loading a specifically formatted excel file for threeme scenarios
#'
#' @description
#' The function loads series from a specified sheet in an excel file, following a determined format. The function also fills missing observations using splines
#'
#' @param excel_sheet file path to the excel file
#' @param sheet_to_load name of the sheet in the excel file with the scenario
#' @param baseline Boolean. Set to true if the sheet is used to calibrate the baseline scenario. Default: FALSE
#' @param base_year Integer. Year of the data calibration
#' @param check_tol Float. Tolerance for the Verification of the match between calibration data and data in the sheet at the baseyear
#' @param stop_if_calib_fail  Boolean. Whether to stop if there is a significant mismatch between original data and excel data. Default: TRUE
#' @param keep_baseyear_calib_data Boolean. If `stop_if_calib_fail` is false and there is a significant mismatch in data, setting to TRUE means the calibration data will be used over the excel data at the base year. Default: TRUE
#' @param calib_baseline Name of the calibrated data for the baseline. Default is the ermeeth2 data name "OGcalib"
#' @param calib_shock Name of the calibrated data after integrating the baseline scenario changes. Default is the ermeeth2 data name "calib_new_base"
#'
#' @returns a data.frame containing the series from the excel sheet, without missing observation
#' @export
#' @import  stringr dplyr
#' @importFrom readxl read_excel
#' @importFrom data.table fread
#' @importFrom purrr map set_names reduce
#' @importFrom crayon yellow bgBlack
#'
#' @examples
#' \dontrun{
#' shock_ch <- load_excel_calibration(excel_sheet = "data/scenarii_inputs.xlsx",
#' sheet_to_load = "shock_NEAM",
#' stop_if_calib_fail = FALSE,
#' check_tol = 10e-8,
#' keep_baseyear_calib_data = TRUE)
#' }
load_excel_calibration<- function(excel_sheet = "configuration/scenarii_calib/scenarii_inputs.xlsx",
                                  sheet_to_load = "baseline",
                                  baseline = FALSE,
                                  base_year = config$input$baseyear,
                                  check_tol = 10e-8,
                                  stop_if_calib_fail = TRUE,
                                  keep_baseyear_calib_data = TRUE,
                                  calib_baseline = OGcalib,
                                  calib_shock = calib_new_base

){

  wyellow <- function(text){crayon::yellow(crayon::bgBlack(text) )}
  test_base <- function(varName = "base"){tryCatch({get(varName); TRUE}, error = function(e) FALSE) }

  xl_data <- readxl::read_excel(excel_sheet,sheet = sheet_to_load )

  ###Importing and cleaning excel data
  names(xl_data)[c(1:2)]<- c("to_load","variable")
  import_base <- xl_data %>% dplyr::filter(to_load != 0) %>% dplyr::select(-to_load) %>%
    t() %>% as.data.frame()

  years <- row.names(import_base)[-1] %>% as.numeric()
  vars <- import_base[1,] %>% as.vector() %>% purrr::reduce(c) %>% tolower()

  cleaned_base <- import_base[-1,]  %>% as.data.frame() %>%
    dplyr::rename_all(~vars)%>%
    dplyr::mutate(year = years) %>%
    dplyr::mutate_all(~stringr::str_remove_all(.x,"\\s")  %>% as.numeric())

  if(baseline){
    if(test_base("calib_baseline")){
      calib_data <- calib_baseline
    }else{
      calib_data <- data.table::fread("src/compiler/calib.csv", data.table = FALSE) |>
        dplyr::select(-baseyear) |>
        dplyr::mutate(year = year + base_year)
      cat("\nCouldn't find calib_new_base, using the dynamo generated calib.csv file instead.\n")
    }

  }else{
    if(test_base("calib_shock")){
      calib_data <- calib_shock
    }else{
      calib_data <- data.table::fread("src/compiler/calib.csv", data.table = FALSE) |>
        dplyr::select(-baseyear) |>
        dplyr::mutate(year = year + base_year)
      cat("\nCouldn't find calib_new_base, using the dynamo generated calib.csv file instead.\n")
    }
  }


  original_data <- calib_data %>% dplyr::select(year, dplyr::any_of(vars))
  vars_not_prexisting <- setdiff(names(cleaned_base),names(original_data))

  if(length(vars_not_prexisting)>0){
    cat(paste0("\nThe following variables were found in the excel sheet but not in calib files:\n"))
    cat(vars_not_prexisting, sep = "\n")
    cat("\nThey will be added anyways\n")}


  ##baseyear check
  check <- original_data[which(original_data$year==base_year),] - (cleaned_base %>% dplyr::select(dplyr::all_of(names(original_data))) )[which(cleaned_base$year==base_year),]

  test <- check[which(abs(check)>check_tol)] %>% names()
  if(length(test)>0){

    cat(wyellow("\nThe following variables do not match calibrated data at base year:\n") )
    cat(test, sep = "\n")
    cat("\n")
    if(stop_if_calib_fail){stop("Mismatch with calibrated data")}else{
      if(keep_baseyear_calib_data){
        cat(wyellow("Continuing replacing with the calibrated data for the baseyear"))
      }else{
        cat(wyellow("Continuing with new data loaded from Excel"))}
    }

  }
  if(keep_baseyear_calib_data){ base_i = 0}else{base_i = 1}

  ###finding which vars to fill
  data_na <- original_data %>% dplyr::mutate_at(setdiff(names(.),"year"), ~ifelse(year<=(base_year-base_i),.x,NA))
  imported_base <- cleaned_base  %>% dplyr::mutate_at(setdiff(names(.),"year"), ~ifelse(year==(base_year*(1-base_i)),NA,.x))

  joined<- dplyr::full_join(data_na,imported_base, by="year")
  vars_to_merge <- names(joined)[which(grepl(".+\\.x$",names(joined)))] %>% stringr::str_remove("\\.x$")

  merged <- purrr::map(vars_to_merge,
                function(vari=.x){
                  joined %>% dplyr::select(dplyr::all_of(c("year",stringr::str_c(vari, c(".x",".y")) ))) %>%
                    dplyr::mutate_at(stringr::str_c(vari,".x") , ~ifelse(is.na(.x),joined[,stringr::str_c(vari,".y")] ,.x)  )
                }) %>% purrr::reduce(left_join,by = "year") %>%
    dplyr::select(dplyr::all_of(c("year",stringr::str_c(vars_to_merge,".x"))))
  names(merged) <-   names(merged) %>% stringr::str_remove("\\.x$")

  ready_data <-dplyr::full_join(merged,joined, by = "year") %>% dplyr::select(-dplyr::ends_with(".y"))%>% dplyr::select(-dplyr::ends_with(".x"))

  ### FILLING IN THE BLANKS
  vars_to_fill <- names(ready_data)[which( (ready_data %>% purrr::map(~sum(is.na(.x))) %>% purrr::reduce(c))>0)]

  if(length(vars_to_fill >0))

  {filler <- vars_to_fill %>% purrr::map(
    function(vari=.x){
      years_b = ready_data$year
      plop <- ready_data %>% dplyr::select(dplyr::all_of(c("year",vari))) %>%
        dplyr::filter_all(~!is.na(.x))
      interpolation_series ( date_vector = plop[,"year"],
                                      value_vector = plop[,vari],
                                      first.date = min(years_b),
                                      last.date = max(years_b))
    }

  ) %>% purrr::set_names(vars_to_fill) %>%  purrr::reduce(cbind) %>% as.data.frame()%>% purrr::set_names(vars_to_fill) %>%
    dplyr::mutate(year = ready_data$year)

  ### completed data
  complete_data <- ready_data %>% dplyr::select(-dplyr::all_of(vars_to_fill)) %>% dplyr::full_join(filler, by="year")
  }else{
    complete_data <- ready_data
  }

  res<- complete_data

}
