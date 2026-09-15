
##fonciton long_data

# data <- readRDS("tests/oilprice_fra_c28_s32.rds")
#
# variables <- c("GDP",variables_like(data,"_C[a-zA-Z0-9]{3}(_S[a-zA-Z0-9]{3})?$",view = FALSE))
# variables <- c("GDP",variables_like(data,"^Y_",view = FALSE))
#
# all_variables <- unique(data$variable)
# my_variables<- sample(all_variables, size = 10000)
#
# data <- data %>% filter(variable %in% variables) %>% filter(is.na(commodity)) %>%
#   mutate(
#   commodity = ifelse(grepl("_C[a-zA-Z0-9]{3}(_S[a-zA-Z0-9]{3})?$",variable),
#                      str_replace_all(variable,"^.+_(C[a-zA-Z0-9]{3})(_S[a-zA-Z0-9]{3})?$","\\1"),
#                      commodity)
# )
# out_format = "list"
# scenarios = c("baseline","oilprice_fra")
#
#
# plop <- wide_data(data = data,
#                   scenarios = scenarios,
#                   variables = variables,
#                   out_format = "list")
# data = plop
# data = ploplist



#' Convert back a wide ThreeMe data.frame into a long ThreeMe data.frame
#'
#' @param data Wide ThreeMe data.frame or list of wide ThreeMe data.frame as created by the wide_data function
#' @param sector_names_table names table for sectors as found in codenames_xxx_xxx.R files. if NULL, sector names will rename coded
#' @param commodity_names_table names table for commodity as found in codenames_xxx_xxx.R files. if NULL, commodity names will rename coded
#'
#' @return a long form ThreeMe dataframe
#' @export
#'
#' @import stringr dplyr
#' @importFrom purrr set_names map imap reduce
#' @importFrom tidyr pivot_longer
#' @importFrom  stats na.omit
#'
#'
long_data <- function(data,
                      sector_names_table = names_sectors,
                      commodity_names_table = names_commodities){
#
#   names_sectors <- NULL
#   names_commodities <- NULL
  variable <- NULL
  sector <- NULL
  commodity <- NULL

  ## Checking data class and scenarios

  if(inherits(data,"data.frame")){

    scenarios <- str_extract(names(data),"^.+\\.") %>% str_remove("\\.") %>% unique() %>% stats::na.omit()

    if (length(scenarios)==0){
      stop(message = "Could not find scenario names. Make sure you use a wide ThreeMe data.frame with scenario names attached to the variables i.e. 'baseline.GDP'.")
    }

    data_list <- purrr::set_names(scenarios)  %>%
      purrr::map(~data %>%
                   dplyr::select(year, dplyr::starts_with(paste0(.x,"."))) %>%
                   dplyr::rename_all(~str_remove(.x,"^.+\\.")) ## should change to use scenario.name
      )
  }else{
    if(inherits(data,"list")){
      data_list <- data
      scenarios <- names(data_list)
    }else{
      stop(message = "data must be a ThreeMe data.frame or a ThreeMe data list created by wide.data.")
    }
  }

  long_data <- data_list %>% purrr::imap(~.x  %>%
                                    tidyr::pivot_longer(cols = !year,
                                                        names_to = "variable") %>%
                                      rename(!!.y := value)  ) %>%
    purrr::reduce(full_join,by = c("year","variable")) %>%
    mutate(sector    = as.character(NA),
           commodity = as.character(NA)) %>%
    mutate(sector = ifelse(grepl("_S[a-zA-Z0-9]{3}$",variable),
                           str_replace_all(variable,"^.+_(S[a-zA-Z0-9]{3})$","\\1"),
                           sector) ,
           commodity = ifelse(grepl("_C[a-zA-Z0-9]{3}(_S[a-zA-Z0-9]{3})?$",variable),
                              str_replace_all(variable,"^.+_(C[a-zA-Z0-9]{3})(_S[a-zA-Z0-9]{3})?$","\\1"),
                              commodity)  ,

           commodity = ifelse(commodity == "CONS",  ## C'est pas propre mais bon c'est la seule exception
                              as.character(NA),
                              commodity)
           )%>% arrange(variable)

  if(!is.null(sector_names_table )){
    long_data <- long_data %>% mutate(
      sector = stringr::str_replace_all(toupper(sector),
                                        purrr::set_names(sector_names_table$name,
                                                         paste0("^",toupper(sector_names_table$code),"$")
                                                         ) ) ) }

  if(!is.null(commodity_names_table )){
    long_data <- long_data %>% mutate(
      commodity = stringr::str_replace_all(toupper(commodity),
                                        purrr::set_names(commodity_names_table$name,
                                                         paste0("^",toupper(commodity_names_table$code),"$")
                                        ) ) )  }


return(long_data %>% as.data.frame() )

}
