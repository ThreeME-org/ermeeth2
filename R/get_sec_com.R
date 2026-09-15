## Extract sectors/commodities from lists


#' Get the sector and commodities codes from an ermeeth2
#'
#' @param list.mdl file path to an mdl list file usually found in the src/model folder
#' @param sectors boolean, to return the sector list, will return a NULL element otherwise. Default is TRUE.
#' @param commodities boolean, to return the commodity list, will return a NULL element otherwise Default is TRUE.
#' @param energy_commodities boolean, to return the energy_commodity list, will return a NULL element otherwise Default is TRUE.
#'
#' @returns a list of vectors for sectors, commodities and energy commodities codes
#' @export
#' @import stringr
#'
#' @examples
#' \dontrun{
#' get_sec_com(list.mdl = file.path("src","model","threeme","R_lists_FRA_c29_s33.mdl"))
#' }
#'
get_sec_com <- function(list.mdl = file.path("src","model","threeme",paste0("R_lists_",config$input$iso3,"_",config$input$classification,".mdl")),
                        sectors = TRUE, commodities = TRUE, energy_commodities = TRUE){

  if(!file.exists(list.mdl)){stop(message(stringr::str_c(" Could not find the specified mdl list :\n", list.mdl, "\n")))}

  list_sec <- NULL
  list_com <- NULL
  list_ce <- NULL

  mdl <- readLines(list.mdl)

  if(sectors == TRUE){
    list_sec <- mdl[which(grepl("%list_sec\\s*:=",mdl))] |>
      stringr::str_remove("%list_sec\\s*:=") |>
      stringr::str_replace_all("\\s+","@") |>
      stringr::str_remove("^@") |> stringr::str_remove("@$")  |>
      stringr::str_split("@") |>  unlist()
  }
  if(commodities == TRUE){
    list_com <- mdl[which(grepl("%list_com\\s*:=",mdl))] |>
      stringr::str_remove("%list_com\\s*:=") |>
      stringr::str_replace_all("\\s+","@") |>
      stringr::str_remove("^@") |> stringr::str_remove("@$")  |>
      stringr::str_split("@") |>  unlist()
  }
  if(energy_commodities == TRUE){
    list_com_E <- mdl[which(grepl("%list_com_E\\s*:=",mdl))] |>
      stringr::str_remove("%list_com_E\\s*:=") |>
      stringr::str_replace_all("\\s+","@") |>
      stringr::str_remove("^@") |> stringr::str_remove("@$")  |>
      stringr::str_split("@") |>  unlist()
  }

  res.list <- list(sectors = list_sec, commodities = list_com, energy_commodities = list_com_E)
  res.list
}


