### Sector plots


### CURVE PLOTS
####################################################################

curve_sc_plot <- function (data, variable, group_type = "sector", scenario = scenario_name, countries = NULL,
                           diff = FALSE, title = "", scenario.names = NULL, startyear = NULL, 
                           endyear = NULL, template = template_default, scenario.diff.ref = "baseline", 
                           growth.rate = FALSE, abs.diff = FALSE, custom_x_breaks = NULL) 
{
  if (is.character(group_type) == FALSE) {
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }
  else {
    group <- toupper(str_replace(group_type, "^(.).*$", "\\1"))
  }
  if (!group %in% c("S", "C")) {
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }
  if (group == "S") {
    division_type = "Sector"
  }
  if (group == "C") {
    division_type = "Commodity"
  }
  if (is.null(scenario)) {
    scenario <- "baseline"
  }
  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 
      0) {
    cat(paste0("Scenario '", scenario, "' was not found in the database. Will plot baseline instead. \n"))
    scenario <- "baseline"
  }
  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 
      0) {
    stop(message = paste0("Scenario '", scenario, "' was not found in the database. \n"))
  }
  if (length(scenario) > 1 & prod(scenario %in% colnames(data)) == 
      0) {
    scenario <- intersect(scenario, colnames(data))
    if (is_empty(scenario)) {
      stop(message = paste0("The specified scenario_name were not found in the database. \n"))
    }
  }
  n.scen <- length(scenario)
  if (is.null(scenario.names)) {
    scenario.names <- gsub("(_|\\.)", " ", scenario)
  }
  if (length(scenario.names) != n.scen) {
    scenario.names <- gsub("(_|\\.)", " ", scenario)
  }
  if (diff == TRUE) {
    if (is.null(scenario.diff.ref)) {
      scenario.diff.ref <- "baseline"
    }
    scenario.diff.ref <- scenario.diff.ref[1]
    if (!scenario.diff.ref %in% colnames(data)) {
      stop(message = paste0("The reference scenario '", 
                            scenario.diff.ref, "' was not found in the database. \n"))
    }
  }
  if (is.null(startyear)) {
    startyear = min(data$year)
  }
  if (startyear < min(data$year)) {
    startyear = min(data$year)
  }
  if (is.null(endyear)) {
    endyear = max(data$year)
  }
  if (endyear > max(data$year)) {
    endyear = max(data$year)
  }
  
  if(is.null(countries)){
    data <- data
  }else{
    data <- data %>% filter(country %in% countries)
  }
  
  years_to_plot <- unique(c(seq(from = startyear, to = endyear, 
                                by = 1)))
  var_vec <- unique(data$variable)
  liste_var <- var_vec[grep(paste0("^", variable, "_", group, 
                                   "[A-Z0-9]{3}$"), var_vec)]
  if (length(liste_var) == 0) {
    liste_var
    stop(message = "No variables matching the variable and the group_type were found.\n")
  }
  if (growth.rate == TRUE) {
    all.scen <- scenario
    if (diff == TRUE) {
      all.scen <- unique(c(all.scen, scenario.diff.ref))
    }
    data <- data %>% dplyr::filter(variable %in% liste_var) %>% 
      dplyr::group_by(variable) %>% dplyr::arrange(variable, 
                                                   year) %>% dplyr::mutate_at(all.scen, ~(.x/dplyr::lag(.x) - 
                                                                                            1)) %>% dplyr::ungroup()
  }
  if (diff == FALSE) {
    graph_data <- data %>% dplyr::filter(variable %in% liste_var) %>% 
      dplyr::filter(year %in% years_to_plot) %>% tidyr::pivot_longer(cols = all_of(scenario)) %>% 
      dplyr::mutate(grouping = paste0(variable, "_", name))
  }
  else {
    if (abs.diff == FALSE) {
      graph_data <- data %>% dplyr::filter(variable %in% 
                                             liste_var) %>% dplyr::filter(year %in% years_to_plot) %>% 
        dplyr::mutate_at(., scenario, ~((.x/get(scenario.diff.ref)) - 
                                          1)) %>% tidyr::pivot_longer(cols = all_of(scenario)) %>% 
        dplyr::mutate(grouping = paste0(variable, "_", 
                                        name))
    }
    else {
      graph_data <- data %>% dplyr::filter(variable %in% 
                                             liste_var) %>% dplyr::filter(year %in% years_to_plot) %>% 
        dplyr::mutate_at(., scenario, ~(.x - get(scenario.diff.ref))) %>% 
        tidyr::pivot_longer(cols = all_of(scenario)) %>% 
        dplyr::mutate(grouping = paste0(variable, "_", 
                                        name))
    }
  }
  graph_data <- as.data.frame(graph_data)
  graph_data$CAT <- graph_data[, tolower(division_type)]
  graph_data$name <- stringr::str_replace_all(graph_data$name, 
                                              purrr::set_names(scenario.names, scenario))
  if (is.null(custom_x_breaks)) {
    n_years <- endyear - startyear
    algo_x_breaks <- 10
    if (n_years <= 35) {
      algo_x_breaks <- 5
    }
    if (n_years <= 20) {
      algo_x_breaks <- 2
    }
    if (n_years <= 10) {
      algo_x_breaks <- 1
    }
    break_x_sequence <- seq(from = startyear, to = endyear, 
                            by = algo_x_breaks)
  }
  else {
    if (is.numeric(custom_x_breaks)) {
      break_x_sequence <- seq(from = startyear, to = endyear, 
                              by = custom_x_breaks)
    }
    else {
      break_x_sequence <- waiver()
    }
  }
  color_outerlines = "gray42"
  color_gridlines = "gray84"
  if (n.scen > 1) {
    res_plot <- ggplot2::ggplot(data = graph_data, aes(group = c(grouping), 
                                                       y = value, x = year, color = CAT)) + ggplot2::geom_point(size = 0) + 
      ggplot2::geom_line(aes(linetype = name)) + ggplot2::labs(linetype = "Scenario") + 
      ggplot2::scale_linetype_manual(values = c("dashed", 
                                                "solid"))
  }
  else {
    res_plot <- ggplot2::ggplot(data = graph_data, aes(group = c(grouping), 
                                                       y = value, x = year, color = CAT)) + ggplot2::geom_point(size = 0) + 
      ggplot2::geom_line()
  }
  res_plot <- res_plot + ggplot2::labs(title = title, color = "") + 
    ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::scale_color_brewer(palette = "Dark2") + 
    ggplot2::theme(legend.position = "bottom")
  if (template == "ofce") {
    res_plot <- res_plot + ofce::theme_ofce(base_family = "")
  }
  if (growth.rate == TRUE & abs.diff == FALSE) {
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }
  if (growth.rate == TRUE & abs.diff == TRUE) {
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format(suffix = ""))
  }
  if (growth.rate == FALSE & abs.diff == FALSE & diff == TRUE) {
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }
  res_plot <- res_plot + ggplot2::scale_x_continuous(breaks = break_x_sequence) + 
    theme(axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), axis.title.x.top = element_blank(), 
          axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank(), 
          axis.line.x.top = element_line(colour = color_outerlines, 
                                         size = 0.5), axis.line.y.right = element_line(colour = color_outerlines, 
                                                                                       size = 0.5), axis.line.x.bottom = element_line(colour = color_outerlines, 
                                                                                                                                      size = 0.5), axis.line.y.left = element_line(colour = color_outerlines, 
                                                                                                                                                                                   size = 0.5), panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour = color_gridlines, 
                                                                                  size = 0.5, linetype = "dashed"), panel.grid.minor.y = element_line(colour = color_gridlines, 
                                                                                                                                                      size = 0.5, linetype = "dashed"), axis.ticks = element_line(size = 0.5, 
                                                                                                                                                                                                                  colour = "grey42"))
  res_plot
}
