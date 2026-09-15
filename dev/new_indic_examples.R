library(tidyverse)
load("tests/newindicators.rda")
scenario_name = "carbontax_lux"


### 0. Data base for testing purposes
variables_selection <-   c(  variables_like(new_indicators_bis,"^EMS_CI_C0",FALSE),
                             variables_like(new_indicators_bis,"^EMS_CH_C0",FALSE),
                             "EMS_CI", "EMS_CH"  )
list_data <- new_indicators_bis %>% wide_data(variables = variables_selection , out_format = "list")
data <- list_data$baseline


### 1. Start by creating a list of commodity where calculation is possible

existing_commodities_CI <- names(data)[grep(pattern = "^EMS_CI_C\\d{3}$" , x= names(data))] %>%
  str_extract("C\\d{3}$")
existing_commodities_CH <- names(data)[grep(pattern = "^EMS_CH_C\\d{3}$" , x= names(data))] %>%
  str_extract("C\\d{3}$")

existing_commodities <- unique(c(existing_commodities_CI,existing_commodities_CH))



### 2.A  Version A : WORKS IF BOTH CH AND CI ARE PRESENT

compute_EMS <- function(commodity, database){
  res <- mutate(database,
                !!paste0("EMS_",commodity) := get(paste0("EMS_CI_",commodity)) + get(paste0("EMS_CH_",commodity)) ) %>%
    select(year,all_of(paste0("EMS_",commodity) ) )
  }

data_res <- existing_commodities %>%
  map(~compute_EMS(commodity = .x, database = data) ) %>%
  reduce(full_join,by = "year") %>%
  full_join(data, by = "year")

##############
### 2.B  Version B : WORKS IF ONE IS MISSING
data_mod <- data %>% select(-EMS_CH_C003,-EMS_CI_C007)

compute_EMS_safe <- function(commodity, database){

  if(exists(paste0("EMS_CI_",commodity), database ) & exists(paste0("EMS_CH_",commodity), database )  ){
  res <- mutate(database,
                !!paste0("EMS_",commodity) := get(paste0("EMS_CI_",commodity)) + get(paste0("EMS_CH_",commodity)) ) %>%
    select(year,all_of(paste0("EMS_",commodity) ) ) }


  if(exists(paste0("EMS_CI_",commodity), database ) & !exists(paste0("EMS_CH_",commodity), database )  ){
    res <- mutate(database,
                  !!paste0("EMS_",commodity) := get(paste0("EMS_CI_",commodity))  ) %>%
      select(year,all_of(paste0("EMS_",commodity) ) ) }


  if(!exists(paste0("EMS_CI_",commodity), database ) & exists(paste0("EMS_CH_",commodity), database )  ){
    res <- mutate(database,
                  !!paste0("EMS_",commodity) := get(paste0("EMS_CH_",commodity)) ) %>%
      select(year,all_of(paste0("EMS_",commodity) ) ) }

  return(res)
}

### 2.C  Version C : WORKS FOR SUMS ONLY

compute_EMS_safe_short <- function(commodity, database){

 var_to_sum <- unique(c(names(database)[grep(paste0("^EMS_CI_",commodity,"$"),names(database))],
                        names(database)[grep(paste0("^EMS_CH_",commodity,"$"),names(database))])
                      )

 res <- mutate(database,
                  !!paste0("EMS_",commodity) := rowSums(across(all_of(var_to_sum))) ) %>%
      select(year,all_of(paste0("EMS_",commodity) ) )

  return(res)
}

# ###Testeur
# database<- data_mod
# commodity = "C001"
# database_test<- mutate(database, plop = rowSums(across(all_of(var_to_sum))) )
# ###

data_res2 <- existing_commodities %>%
  map(~compute_EMS_safe_short(commodity = .x, database = data_mod) ) %>%
  reduce(full_join,by = "year") %>%
  full_join(data_mod,., by = "year")

