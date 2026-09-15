
# function contrib
## scenario can either be string of length one : "baseline" or a scenario in the database , OR
## two scenario in which cas the difference will be calculated

#' Compute the contributions of component variables to the growth (or diff growth) of an aggregated variable
#'
#' @param data a ThreeMe long form data.frame
#' @param var1 character (length 1) the aggregate variable
#' @param var2 character vector. component variables of the aggregate variable
#' @param scenar name of scenarios (either one unique scenario or one scenario + baseline), must be in the data
#' @param indicator  character (length 1) the choice of indicator (relative deviation rel.diff, growth rate difference; gr.rel)
#' @param neg.value component variables which contribution should be substracted i.e. imports on GDP
#' @param check_digit numeric (length 1) precision of the check
#'
#' @return a data.frame
#' @export
#' @import dplyr
#' @import tidyr
#'
contrib <- function(data,
                    var1,
                    var2,
                    indicator = "rel.diff",
                    scenar = scenarios,
                    check_digit = 3,
                    neg.value = NULL )
{

  # scenarios <- NULL
  # variable <- NULL
  # baseline <- NULL
  # . <- NULL
  # scenario <- NULL
  # lag.value <- NULL

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

  # if (prod(scenar %in% names(data)) == 0 ) {
  #   not_found <- setdiff(scenar, names(data))
  #   stop(message = paste0("The '",not_found,"' scenario was not found in the database.\n"))
  # }
  ####

  filtered.var <- c(var1,var2)

  data.in <- data %>% dplyr::filter(variable %in% filtered.var)


  # Weigthts in the baseline scenario
  data.w_baseline <- data.in  %>%
    dplyr::select(variable, year, baseline) %>%
    tidyr::pivot_wider(names_from = variable,
                       values_from = baseline) %>%
    dplyr::mutate_at(.funs = list(weight = ~./get(var1)), .vars = var2) %>%
    dplyr::select(year, contains("_weight"))

  if (!is.null(neg.value)){
    data.w_baseline  <- data.w_baseline %>% dplyr::mutate_at(str_c(neg.value,"_weight") , ~((-1) *.x))
  }

  if (length(scenar) == 1) {
    data.contrib.1 <- data.in %>%  dplyr::mutate(scenario = .[,scenar]) %>%
      dplyr::select(variable, year, scenario) %>%
      tidyr::pivot_wider(names_from = variable,
                         values_from = c(scenario))

    data.contrib.2 <- data.contrib.1

  } else {

    shock_scenario <- setdiff(scenar,"baseline")


    ## Diff absolue
    data.contrib.1 <- data.in %>%  dplyr::mutate(scenario = .[,str_c(shock_scenario)] - .[,"baseline"]) %>%
      dplyr::select(variable, year, scenario) %>%
      tidyr::pivot_wider(names_from = variable,
                         values_from = scenario)

    ## Diff relative
    data.contrib.2 <- data.in %>%  dplyr::mutate(scenario = .[,str_c(shock_scenario)] / .[,"baseline"] -1) %>%
      dplyr::select(variable, year, scenario) %>%
      tidyr::pivot_wider(names_from = variable,
                  values_from = scenario)
  }



  #Calcul des contributions a base de mutate_at


  if(indicator == "rel.diff" & indicator != "gr.diff" & indicator != "share"){

    # Imports en négatif
    if (!is.null(neg.value)){
      data.contrib.1 <- data.contrib.1 %>% dplyr::mutate_at(neg.value , ~((-1) *.x))
    }

    # Check on the weights
    weight_check<-  data.contrib.1 %>%
      dplyr:: mutate_at(.funs = list(weight = ~./get(var1)), .vars = var2) %>%
      dplyr::select(year, contains("_weight")) %>%
      as.data.frame() %>% `colnames<-`(c("year",var2))  %>%
      tidyr::pivot_longer(names_to = "variable", values_to = "value", -year)  %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::select(-year) %>% rowSums()


    # Variation for var 2 variables
    data.contrib.3 <- data.contrib.2 %>% dplyr::select(-all_of(var1))

    data.contrib <-  (dplyr::select(data.contrib.3, year, all_of(var2))[-1] * dplyr::select(data.w_baseline,year, all_of(str_c(var2,"_weight")))[-1]) %>%
      cbind("year" = data.contrib.2[1], .) %>% as.data.frame() %>%
      tidyr::pivot_longer(names_to = "variable", values_to = "value", - year)



  }
  else {

    if(indicator == "gr.diff"  & indicator != "share") {

      if (length(scenar) == 1) {
        # Taux de croissance annuel pour chaque variable et période (scenar)
        data.gr_sc <- data.in %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(lag.value = get(scenar) - dplyr::lag(get(scenar), n = 1, default = NA),
                        value = ((lag.value  / get(scenar)))) %>%
          dplyr::select(year, variable, value) %>%
          tidyr::pivot_wider(names_from = variable, values_from = value)

        df <-  (dplyr::select(data.gr_sc, year, all_of(var2))[-1] * data.w_baseline[-1] )

        data.contrib <-  cbind("year" = data.gr_sc[1], df) %>% as.data.frame() %>%
          tidyr::pivot_longer(names_to = "variable", values_to = "value", - year)

      } else {
        shock_scenario <- setdiff(scenar,"baseline")

        data.gr_baseline <- data.in %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(lag.value = baseline - dplyr::lag(baseline, n = 1, default = NA),
                        value = ((lag.value  / baseline))) %>%
          dplyr::select(year, variable, value) %>%
          tidyr::pivot_wider(names_from = variable, values_from = value)

        data.gr_sc <- data.in %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(lag.value = get(shock_scenario) - dplyr::lag(get(shock_scenario), n = 1, default = NA),
                        value = ((lag.value  / get(shock_scenario)))) %>%
          dplyr::select(year, variable, value) %>%
          tidyr::pivot_wider(names_from = variable, values_from = value)

        df <-  (dplyr::select(data.gr_sc, year, all_of(var2))[-1] * data.w_baseline[-1]) - (dplyr::select(data.gr_baseline, year, all_of(var2))[-1] * data.w_baseline[-1])


        data.contrib <-  cbind("year" = data.gr_baseline[1], df) %>% as.data.frame() %>%
          tidyr::pivot_longer(names_to = "variable", values_to = "value", - year)
      }

      weight_check <-  data.w_baseline %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::select(-year) %>% rowSums()

    } else {
      if(indicator == "share"){

        data.contrib <- data.in  %>%
          dplyr::select(variable, year, scenar) %>%
          tidyr::pivot_wider(names_from = variable,
                             values_from = scenar) %>%
          dplyr::mutate_at(.funs = list(weight = ~./get(var1)), .vars = var2) %>%
          dplyr::select(year, contains("_weight")) %>%
          `colnames<-`(c("year",var2)) %>%
          tidyr::pivot_longer(names_to = "variable", values_to = "value", - year)


      } else{
        data.contrib <- data.contrib.1 %>% as.data.frame() %>%
          dplyr::select(-var1,year, var2) %>%
          `colnames<-`(c("year",var2)) %>%
          tidyr::pivot_longer(names_to = "variable", values_to = "value", - year)
      }
      weight_check <-  data.w_baseline %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::select(-year) %>% rowSums()

    }

  }


  ## Sanity check message
  if(round(weight_check,check_digit) != 1){
    cat(str_c("Weights are not summing to one: Try again !\n (difference of: ", 100 * (round(weight_check,check_digit)- 1),"%)"))
  } else{
    cat("Weights sum to one: Good job !\n")
  }
  ##
  data.contrib
}
