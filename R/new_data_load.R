
#' Read the eviews csv output from simulations
#'
#' @param csv.file.path file path to the EViews simulation csv output
#' @param variables_selection vector of variables
#' @param scenario_label name to give to the scenario, if different from the basename of the csv file. Default is NULL and uses the basename of the csv.file
#'
#' @importFrom data.table fread
#' @importFrom stats na.omit
#' @returns a data.frame containing the simulation output
#' @export
#' @import stringr dplyr
#' @importFrom purrr map keep set_names
#' @importFrom data.table melt fread
#' @importClassesFrom data.table data.table
#'
#'
read_3me_eviews_csv <- function(csv.file.path = file.path("csv", "ct1.csv"),
                                variables_selection = NULL,
                                scenario_label = NULL){

  # csv.file.path = c(file.path("data","temp","csv", "ct1.csv"))
  # variables_selection = NULL
  # scenario_label = NULL

  V1 <- NULL
  variable <- NULL

  ### Checks ###
  if(is.null(scenario_label)){
    scenario_label <- stringr::str_remove(basename(csv.file.path),"\\.csv$")
  }

  data_read <- data.table::fread(input = csv.file.path,
                                 data.table = TRUE) |> stats::na.omit()
  ### ### ### ### ### ### ###


  n_scen <- names(data_read |> select(-V1)) |> stringr::str_replace("^.+_(\\d+)$","\\1") |> unique()
  nb <- length(n_scen)

  if(length(scenario_label) < (nb-1) ){
    scenario_label <- stringr::str_c("scen", c(1:(nb-1)))
  }else{
    scenario_label<- scenario_label[1:(nb-1)]
  }

  n_scen <- purrr::set_names(n_scen,c("baseline",scenario_label ))


  ### Variable Selection ###
  if(!is.null(variables_selection)){

    data_read <- data_read |> dplyr::select( dplyr::any_of(c("V1" , map(variables_selection,~stringr::str_c(.x,n_scen,sep = "_")) |> unlist()  ) ) )

  }
  ### ### ### ### ### ### ###



  nn <- names(data_read)
  sel <- purrr::map(n_scen, function(s)purrr::keep(nn, stringr::str_detect(nn, stringr::str_c("_", s,"$") )))

  suppressWarnings({

    data_long3<- data_read |> data.table::melt(id.vars = "V1",
                                               measure= sel,
                                               variable.factor = TRUE
    )
  })

  nsel <- sel[[1]] |> stringr::str_remove('_0$')
  data_res <- data_long3[, variable_n := nsel[variable]] |>
  dplyr::select(-variable) |> dplyr::rename(variable = variable_n, year = V1) |> dplyr::relocate(variable, .after = year) |>
    # mutate(sector = NA_character_, commodity = NA_character_) |>
    as.data.frame()



}

### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### Function to add sector commodidity names ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ###



#' Add sectors and commodities names in the simulation output database based on a names_ files
#'
#' @param data data.frame output of simulations
#' @param commodities_names  data.frame containing codes and names of commodities
#' @param sectors_names data.frame containing codes and names of sectors
#' @param exception_s_c vector fo 4 characters string that are neither sectors or commodities
#'
#' @returns the data where the names of sectors and commodities appear in the sectors and commodities database
#' @export
#'
#' @import stringr dplyr
#'
#'
#'
add_com_sec_names<- function(data  ,
                             commodities_names = names_commodities,
                             sectors_names = names_sectors ,
                             exception_s_c = c("CONS","CONT")
){

  # names_commodities <- NULL
  # names_sectors <- NULL
  variable <- NULL
  s_code <- NULL
  c_code <- NULL
  c_id <- NULL
  s_id <- NULL
  unidentified_s <- NULL
  unidentified_c <- NULL
  s_name <- NULL
  c_name <- NULL


  ## Function to add sector and commodity names via a codename file


  ## 1 Check sectors and commodities present in database

  ### This functions supposes that commodities or sectors appear only once in a variable name (ie VAR_C, var_S or var_C_S )

  variable_frame <- data.frame(variable = data[,"variable"] |> unique() |> toupper() ,
                               unit = 1) |>

    mutate(s_code = stringr::str_extract(variable,"_(S[0-9A-Za-z]{3})$", group=1 ),
           c_code = stringr::str_extract(variable,"_(C[0-9A-Za-z]{3})(_S|$)", group=1  )
    ) |> as.data.frame()

  variable_frame[variable_frame == ''] <- NA
  variable_frame$c_code[variable_frame$c_code %in% exception_s_c] <- NA
  variable_frame$s_code[variable_frame$s_code %in% exception_s_c] <- NA

  ## 2. link to the names data base

  s_names <- sectors_names |> dplyr::rename_all(~stringr::str_c("s_",.x)) |> dplyr::mutate(s_code = toupper(s_code) , s_id =1)
  c_names <- commodities_names |> dplyr::rename_all(~stringr::str_c("c_",.x))|> dplyr::mutate(c_code = toupper(c_code), c_id =1)

  variable_id = variable_frame |>  dplyr::left_join(s_names, by = "s_code") |> dplyr::left_join(c_names, by = "c_code") |>
    dplyr::mutate( unidentified_c = ifelse((!is.na(c_code) & is.na(c_id)), 1 , 0 ),
            unidentified_s = ifelse((!is.na(s_code) & is.na(s_id)), 1 , 0 )
    )

  unidentified <- sum(variable_id$unidentified_c) + sum(variable_id$unidentified_s)


  if( unidentified > 0 ){
    message_warning(" The following sector/commodity codes could not be identified. Either add them to the bridge and code names for the chosen classification or add them to argument `exception_codes` ")

    cat("\n Unidentified sector codes:\n ")
    vars_s <- variable_id |> dplyr::filter(unidentified_s ==1) |> dplyr::select(variable, s_code)
    vars_s$s_code |> unique() |> cat(sep = "  ")

    cat("\n Found on the following variables: \n")
    vars_s$variable |> cat(sep = "\n")

    cat("\n\n Unidentified commodities codes:\n ")
    vars_c<-variable_id |> dplyr::filter(unidentified_c == 1 ) |> dplyr::select(variable, c_code )
    vars_c$c_code |> unique() |> cat(sep = "  ")

    cat("\n Found on the following variables: \n")

    vars_c$variable |> cat(sep = "  ")
  }



  ###. add to full data

  data_res <- data |> dplyr::left_join(variable_id |> dplyr::select(variable, s_name,s_code), by = "variable") |>
    dplyr::left_join(variable_id |> dplyr::select(variable, c_name, c_code), by = "variable") |>
    dplyr::mutate(sector = ifelse(is.na(s_name), s_code,s_name),
           commodity = ifelse(is.na(c_name), s_code,c_name)
    ) |>
    dplyr::select(-s_name, -c_name,  -c_code, -s_code)


}
