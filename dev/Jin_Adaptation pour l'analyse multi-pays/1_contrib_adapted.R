
# variable to decompose
#var1 <- "GDP"

# variables decomposed
#var2 <- c("CH","X","M", "I", "G")


# contrib(data %>% filter(year>2021),"GDP",c( "CH","X","M", "I", "G","DS"), scenar = c("oilprice_fra") )
# contrib(data %>% filter(year>2021),"GDP",c( "CH","X","M", "I", "G","DS"), scenar = c("baseline") )
# contrib(data %>% filter(year>2021),"GDP",c( "CH","X","M", "I", "G","DS") )
#
# contrib(data %>% filter(year>2021),"GDP",c( "CH","X","M", "I", "G","DS"), scenar = c("baseline","oilprice_fra") ) #?

# function contrib
## scenario can either be string of length one : "baseline" or a scenario in the database , OR
## two scenario in which cas the difference will be calculated

contrib <- function (data, var1, var2, 
                     countries = NULL,
                     indicator = "rel.diff", scenar = scenarios, 
                     check_digit = 3, neg.value = "M") 
{
  if (is.null(scenar)) {
    scenar = "baseline"
  }
  if (length(scenar) > 2) {
    stop(message = "Indicate a maximum of two scenarios.\n")
  }
  if (length(scenar) == 2 & !"baseline" %in% scenar) {
    stop(message = "If two scenarios are given, one must be the ' 'baseline' scenario.\n")
  }
  if (prod(scenar %in% names(data)) == 0) {
    not_found <- setdiff(scenar, names(data))
    stop(message = paste0("The '", not_found, "' scenario was not found in the database.\n"))
  }
  filtered.var <- c(var1, var2)
  
  if (is.null(countries)){
    data.in <- data %>% dplyr::filter(variable %in% filtered.var)
  }else{
    data.in <- data %>% dplyr::filter(variable %in% filtered.var,
                                      country %in% countries)
  }

  data.w_baseline <- data.in %>% dplyr::select(variable, year, 
                                               baseline) %>% tidyr::pivot_wider(names_from = variable, 
                                                                                values_from = baseline) %>% dplyr::mutate_at(.funs = list(w = ~./get(var1)), 
                                                                                                                             .vars = var2) %>% dplyr::select(year, contains("w"))
  if (neg.value %in% var2) {
    data.w_baseline <- data.w_baseline %>% dplyr::mutate_at(str_c(neg.value, 
                                                                  "_w"), ~((-1) * .x))
  }
  if (length(scenar) == 1) {
    data.contrib.1 <- data.in %>% dplyr::mutate(scenario = .[, 
                                                             scenar]) %>% dplyr::select(variable, year, scenario) %>% 
      tidyr::pivot_wider(names_from = variable, values_from = c(scenario))
    data.contrib.2 <- data.contrib.1
  }
  else {
    shock_scenario <- setdiff(scenar, "baseline")
    data.contrib.1 <- data.in %>% dplyr::mutate(scenario = .[, 
                                                             str_c(shock_scenario)] - .[, "baseline"]) %>% dplyr::select(variable, 
                                                                                                                         year, scenario) %>% tidyr::pivot_wider(names_from = variable, 
                                                                                                                                                                values_from = scenario)
    data.contrib.2 <- data.in %>% dplyr::mutate(scenario = .[, 
                                                             str_c(shock_scenario)]/.[, "baseline"] - 1) %>% dplyr::select(variable, 
                                                                                                                           year, scenario) %>% pivot_wider(names_from = variable, 
                                                                                                                                                           values_from = scenario)
  }
  if (indicator == "rel.diff" & indicator != "gr.diff") {
    if (neg.value %in% var2) {
      data.contrib.1 <- data.contrib.1 %>% dplyr::mutate_at(neg.value, 
                                                            ~((-1) * .x))
      data.w_baseline <- data.w_baseline %>% dplyr::mutate_at(str_c(neg.value, 
                                                                    "_w"), ~((-1) * .x))
    }
    data.contrib <- data.contrib.1 %>% dplyr::mutate_at(.funs = list(w = ~./get(var1)), 
                                                        .vars = var2) %>% dplyr::select(year, contains("w")) %>% 
      as.data.frame() %>% `colnames<-`(c("year", var2)) %>% 
      tidyr::pivot_longer(names_to = "variable", values_to = "value", 
                          -year)
    weight_check <- data.contrib %>% tidyr::pivot_wider(names_from = variable, 
                                                        values_from = value) %>% dplyr::filter(year == max(year)) %>% 
      dplyr::select(-year) %>% rowSums()
    data.contrib.3 <- data.contrib.2 %>% select(-GDP, year, 
                                                var2)
    data.contrib <- (data.contrib.3[-1] * data.w_baseline[-1]) %>% 
      cbind(year = data.contrib.3[1], .) %>% as.data.frame() %>% 
      tidyr::pivot_longer(names_to = "variable", values_to = "value", 
                          -year)
  }
  else {
    if (indicator == "gr.diff") {
      if (length(scenar) == 1) {
        data.gr_sc <- data.in %>% dplyr::group_by(variable) %>% 
          dplyr::mutate(lag.value = get(scenar) - dplyr::lag(get(scenar), 
                                                             n = 1, default = NA), value = ((lag.value/get(scenar)))) %>% 
          dplyr::select(year, variable, value) %>% tidyr::pivot_wider(names_from = variable, 
                                                                      values_from = value)
        df <- (dplyr::select(data.gr_sc, year, var2)[-1] * 
                 data.w_baseline[-1])
        data.contrib <- cbind(year = data.gr_sc[1], df) %>% 
          as.data.frame() %>% tidyr::pivot_longer(names_to = "variable", 
                                                  values_to = "value", -year)
      }
      else {
        shock_scenario <- setdiff(scenar, "baseline")
        data.gr_baseline <- data.in %>% dplyr::group_by(variable) %>% 
          dplyr::mutate(lag.value = baseline - dplyr::lag(baseline, 
                                                          n = 1, default = NA), value = ((lag.value/baseline))) %>% 
          dplyr::select(year, variable, value) %>% tidyr::pivot_wider(names_from = variable, 
                                                                      values_from = value)
        data.gr_sc <- data.in %>% dplyr::group_by(variable) %>% 
          dplyr::mutate(lag.value = get(shock_scenario) - 
                          dplyr::lag(get(shock_scenario), n = 1, default = NA), 
                        value = ((lag.value/get(shock_scenario)))) %>% 
          dplyr::select(year, variable, value) %>% tidyr::pivot_wider(names_from = variable, 
                                                                      values_from = value)
        df <- (dplyr::select(data.gr_sc, year, var2)[-1] * 
                 data.w_baseline[-1]) - (dplyr::select(data.gr_baseline, 
                                                       year, var2)[-1] * data.w_baseline[-1])
        data.contrib <- cbind(year = data.gr_baseline[1], 
                              df) %>% as.data.frame() %>% tidyr::pivot_longer(names_to = "variable", 
                                                                              values_to = "value", -year)
      }
      weight_check <- data.w_baseline %>% dplyr::filter(year == 
                                                          max(year)) %>% dplyr::select(-year) %>% rowSums()
    }
    else {
      data.contrib <- data.contrib.1 %>% as.data.frame() %>% 
        dplyr::select(-GDP, year, var2) %>% `colnames<-`(c("year", 
                                                           var2)) %>% tidyr::pivot_longer(names_to = "variable", 
                                                                                          values_to = "value", -year)
    }
  }
  if (round(weight_check, check_digit) != 1) {
    cat(str_c("Weights are not summing to one: Try again !\n (difference of: ", 
              100 * (round(weight_check, check_digit) - 1), "%)"))
  }
  else {
    cat("Weights sum to one: Good job !\n")
  }
  data.contrib
}
