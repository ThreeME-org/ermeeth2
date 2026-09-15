#' Contrib.sub computes the contributions of sectors or commodities for a given variable type
#'
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param var1 character(1) string, variable qui seront décomposées en sous-composantes
#' @param group_type character(1), type du sous-groupe c("commodity", "sector")
#' @param scenar character(1), nom du ou des scénarios
#' @param check_digit (1), precision de la vérification
#'
#' @return un dataframe
#' @import dplyr tidyr
#'
#' @export
contrib.sub <- function(data, var1,  group_type = "sector",
                            scenar = scenario_to_analyse,
                            check_digit = 3)
{

  if (is.null(scenar)){
    scenar = "baseline"
  }

  ## further checks on scenarios
  if (length(scenar) > 2){
    stop(message = "Indicate a maximum of two scenarios.\n")
  }

  if (length(scenar) == 2 & !"baseline" %in% scenar){
    stop(message = "If two scenarios are given, one must be the ' 'baseline' scenario.\n")
  }

  if (prod(scenar %in% names(data)) == 0 ) {
    not_found <- setdiff(scenar, names(data))
    stop(message = paste0("The '",not_found,"' scenario was not found in the database.\n"))
  }

  if (is.null(check_digit)){
    check_digit = 3
  }


  if(length(scenar)==2){
    contrib_ecart = TRUE
  }else{ contrib_ecart = FALSE}


  #### Checking group_type
  if(is.character(group_type)== FALSE){
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }else{
    group <- toupper(str_replace(group_type,"^(.).*$","\\1" ))
  }

  if(!group %in% c("S", "C")){
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }

  if(group == "S"){division_type = "Sector" }
  if(group == "C"){division_type = "Commodity" }


  #### Check that the variables exists
  var_vec <- unique(data$variable)

  liste_var <- var_vec[grep(paste0("^",var1,"_",group,"[A-Z0-9]{3}$"),var_vec)]

  filtered.var <-  dplyr::filter(data,variable %in% liste_var)
  filtered.val <- unique(filtered.var$variable[filtered.var$baseline != 0])


  if(length(liste_var) == 0 ){
    liste_var
    stop(message = "No variables matching the variable and the group_type were found.\n") }

  data.contrib.0 <- data %>% dplyr::filter(variable %in% c(var1, filtered.val)) #%>%
  #select(-subsector, -subcommodity, -subcommodity2, -subcommodity3,-commodity, -commodity2, -commodity3, -sector,-variable_root)
  #select(year, variable, scenar)

  data.contrib.lbl <- data %>% dplyr::filter(variable %in% c(var1, filtered.val)) %>%
    select(variable,year, commodity, sector)

  if(group == "C"){data.contrib.lbl <- select(data.contrib.lbl, -sector) %>%
    `colnames<-`(c("variable", "year", "label")) }
  if(group == "S"){data.contrib.lbl <- select(data.contrib.lbl, -commodity) %>%
    `colnames<-`(c("variable", "year", "label"))}


  # Weigthts in the baseline scenario
  data.w_baseline <- data.contrib.0 %>%
    select(variable, year, baseline) %>%
    pivot_wider(names_from = variable,
                values_from = baseline) %>%
    mutate_at(.funs = list(w = ~./get(var1)), .vars = filtered.val) %>%
    select(year, contains("_w"))



  # Calcul of shares for one scenario
  if (length(scenar) == 1) {
    data.contrib.1 <- data.contrib.0 %>%  mutate(scenario = .[,scenar]) %>%
      select(variable, year, scenario) %>%
      pivot_wider(names_from = variable,
                  values_from = c(scenario))



    data.contrib <- data.contrib.1 %>%
      mutate_at(.funs = list(w = ~./get(var1)), .vars = filtered.val) %>%
      select(year, contains("_w"))

    weight_check <-  round(rowSums(data.contrib[10,]) - data.contrib[10,1], check_digit)

    data.contrib <- data.contrib %>%
      as.data.frame() %>% `colnames<-`(c("year", unique(filtered.val))) %>%
      pivot_longer(names_to = "variable", values_to = "value", - year)

    data.contrib <-  left_join(data.contrib.lbl, data.contrib, by = c("variable","year"))


  } else {
    shock_scenario <- setdiff(scenar,"baseline")


    ## Diff absolue
    data.contrib.1 <- data.contrib.0 %>%
      mutate(scenario = .[,str_c(shock_scenario)] - .[,"baseline"]) %>%
      select(variable, year, scenario) %>%
      pivot_wider(names_from = variable,
                  values_from = scenario)

    ## Diff relative
    data.contrib.2 <- data.contrib.0 %>%  mutate(scenario = .[,str_c(shock_scenario)] / .[,"baseline"] -1) %>%
      select(variable, year, scenario) %>%
      pivot_wider(names_from = variable,
                  values_from = scenario)

    # ## Calcul des contributions
    data.contrib <- data.contrib.1 %>%
      mutate_at(.funs = list(w = ~./get(var1)), .vars = filtered.val) %>%
      select(year, contains("_w"))

    weight_check <-  round(rowSums(data.contrib[10,]) - data.contrib[10,1], check_digit)

    data.contrib.3 <- data.contrib.2 %>% select(-var1)

    data.contrib.4 <-  (data.contrib.3[-1] * data.w_baseline[-1]) %>%
      cbind("year" = data.contrib.3[1],select(data.contrib.2, var1), .) %>% as.data.frame() %>%
      pivot_longer(names_to = "variable", values_to = "value", - year)

    data.contrib <- left_join(data.contrib.lbl, data.contrib.4, by = c("variable","year"))
  }


  # A complter pour calcul de contribution en diff de taux de croissance
  # data.contrib %>% mutate_at(vars(-("year")),lag) %>%
  #select(year, contains("contrib"))  %>% as.data.frame(col.names = c("year",var2)) %>% `colnames<-`(c("year",var2)) %>%
  #pivot_longer(names_to = "variable", values_to = value, - year)

  ## Warning message
  if(weight_check != 1){
    cat("Weights are not summing to one: Try again !\n")
  } else{
    cat("Weights sum to one: Good job !\n")
  }

  data.contrib
}







