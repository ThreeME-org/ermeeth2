### Sector plots


### CURVE PLOTS
####################################################################

#' Draw curve plots for a variable decomposed into multiple sectors or commodities
#'
#' @param data a ThreeMe data.frame in long form
#' @param variable character string (1) with the root variable to plot. It must be present in the data in both aggregated form and in multiple sector/commodity form. i.e. for output, specify "Y"
#' @param group_type length 1 "sector" or "commodity" , "S" or "C" also works
#' @param scenario the scenario to plot, must be present in the data
#' @param diff If TRUE, the difference from baseline scenario will be plotted. Default is FALSE
#' @param title Title of plot to display
#' @param scenario.names Name of the scenario to display. If NULL the scenario variable will be used
#' @param startyear First year to plot. If NULL (default) reverts to earliest year available.
#' @param endyear Last year to plot. If NULL (default) reverts to lastest year available.
#' @param template Theme to use (OFCE or nothing)
#' @param scenario.diff.ref Name of the reference scenario from which to compute the difference. Default is "baseline"
#' @param growth.rate Whether to plot growth rate or not. Default is FALSE (ie will plot levels)
#' @param abs.diff If plot is in difference TRUE will plot absolute difference, FALSE (default) will plot relative difference
#' @param custom_x_breaks integer(1) permet de choisir manuellement les ticks des années à afficher. Si NULL (défaut) alors utilise l'algorithme de la fonction. Si "R" : algorithme par défaut de R sera utilisé
#'
#' @import ggplot2 dplyr tidyr ofce
#'
#' @importFrom scales percent label_number percent_format
#' @importFrom purrr map set_names
#'
#' @return a ggplot
#' @export
#'

curve_sc_plot <- function(data , variable, group_type = "sector",
                          scenario = scenario_to_analyse,
                          diff = FALSE ,
                          title = "" ,
                          scenario.names = NULL,
                          startyear = NULL, endyear = NULL,
                          template = template_default,
                          scenario.diff.ref = "baseline", growth.rate = FALSE, abs.diff = FALSE,
                          custom_x_breaks = NULL){

  # scenario_to_analyse <- NULL
  template_default <- NULL
  name <- NULL
  . <- NULL
  sector <- NULL
  commodity <- NULL
  CAT <- NULL


  #########################
  ### 0. Running Checks ###
  #########################
  # browser()
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


  #### scenario_to_analyse check
  if (is.null(scenario)){
    scenario <- "baseline"
  }

  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 0 ){
    cat(paste0("Scenario '",scenario,"' was not found in the database. Will plot baseline instead. \n"   ))
    scenario <- "baseline"
  }

  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 0){
    stop(message = paste0("Scenario '",scenario,"' was not found in the database. \n"   ))
  }

  if(length(scenario) > 1 & prod(scenario %in% colnames(data)) == 0){
    scenario <- intersect(scenario , colnames(data))
    if(purrr::is_empty(scenario)){ stop(message = paste0("The specified scenario_to_analyse were not found in the database. \n"   )) }
  }

  n.scen <- length(scenario)

  if(is.null(scenario.names)){scenario.names <- gsub("(_|\\.)"," ",scenario)}
  if(length(scenario.names) != n.scen){scenario.names <- gsub("(_|\\.)"," ",scenario)}


  #### diff checks
  if(diff == TRUE){
    if(is.null(scenario.diff.ref)){scenario.diff.ref <- "baseline"}
    scenario.diff.ref <- scenario.diff.ref[1]

    if(!scenario.diff.ref %in% colnames(data)){
      stop(message = paste0("The reference scenario '",scenario.diff.ref , "' was not found in the database. \n"   ) )
    }
  }

  #### Determining years to plot


  if(is.null(startyear) ){
    startyear = min(data$year)
  }

  if(startyear < min(data$year)){
    startyear = min(data$year)
  }

  if(is.null(endyear) ){
    endyear = max(data$year)
  }

  if(endyear > max(data$year)){
    endyear = max(data$year)
  }


  years_to_plot <- unique(c(seq(from = startyear, to  = endyear, by  = 1)))

  #############################
  ### 1. Preparing the data ###
  #############################

  #### Check that the variables exists
  var_vec <- unique(data$variable)

  liste_var <- var_vec[grep(paste0("^",variable,"_",group,"[A-Z0-9]{3}$"),var_vec)]

  if(length(liste_var) == 0 ){
    liste_var
    stop(message = "No variables matching the variable and the group_type were found.\n") }

  #### Building the data_base

  ##### Calcuting growth rates

  if(growth.rate == TRUE){
    all.scen <- scenario
    if(diff == TRUE){all.scen <- unique(c(all.scen,scenario.diff.ref))}

    data <-data %>%
      dplyr::filter(variable %in% liste_var) %>%
      dplyr::group_by(variable) %>%
      dplyr::arrange(variable, year) %>%
      dplyr::mutate_at(all.scen,~(.x/dplyr::lag(.x) - 1)) %>%
      dplyr::ungroup()
  }

  ##### No diffs

  if(diff == FALSE){
    graph_data <- data %>%

      dplyr::filter(variable %in% liste_var) %>%
      dplyr::filter(year %in% years_to_plot) %>%
      tidyr::pivot_longer(cols = all_of(scenario))  %>%
      dplyr::mutate(grouping = paste0(variable,"_",name))
  }else{

    ##### With diffs
    if (abs.diff== FALSE){
      graph_data <- data %>%

        dplyr::filter(variable %in% liste_var) %>%
        dplyr::filter(year %in% years_to_plot) %>%
        dplyr::mutate_at(.,scenario, ~((.x/get(scenario.diff.ref))-1))  %>%
        tidyr::pivot_longer(cols = all_of(scenario))  %>%
        dplyr::mutate(grouping = paste0(variable,"_",name))
    }else{
      graph_data <- data %>%

        dplyr::filter(variable %in% liste_var) %>%
        dplyr::filter(year %in% years_to_plot) %>%
        dplyr::mutate_at(.,scenario, ~(.x-get(scenario.diff.ref)))  %>%
        tidyr::pivot_longer(cols = all_of(scenario))  %>%
        dplyr::mutate(grouping = paste0(variable,"_",name))
    }




  }

  graph_data <- as.data.frame(graph_data)
  graph_data$CAT <- graph_data[,tolower(division_type)]
  graph_data$name <- stringr::str_replace_all(graph_data$name, purrr::set_names(scenario.names,scenario))

  series <- unique(graph_data$variable) %>% sort
  label <- unique(graph_data$CAT) %>% sort


  #### Preparing x axis breaks
  ## calculating the optimal number of ticks to show

  if(is.null(custom_x_breaks)){
    n_years <- endyear - startyear
    algo_x_breaks <- 10

    if(n_years <= 35){ algo_x_breaks <- 5 }
    if(n_years <= 20){ algo_x_breaks <- 2 }
    if(n_years <= 10){ algo_x_breaks <- 1 }

    break_x_sequence <- seq(from = startyear, to =  endyear, by = algo_x_breaks)

  }else{
    if(is.numeric(custom_x_breaks)){
      break_x_sequence <- seq(from =  startyear, to = endyear, by = custom_x_breaks)

    }else{
      break_x_sequence <- waiver()}

  }


  ########################
  ### 2. Drawing Plots ###
  ########################
  color_outerlines = "gray42"

  color_gridlines = "gray84"

  #### Creating the color palette
  if(group== "S"){
    data_sectors <-unique(data %>% dplyr::select(sector) %>% dplyr::filter(!is.na(sector)) ) %>% unlist() %>% unname()
    n_sector<-length(data_sectors)


    palette <- custom.palette(n = n_sector) %>% purrr::set_names(.,data_sectors)

  }


  if(group== "C"){
    data_commodities <-unique(data %>% dplyr::select(commodity) %>% dplyr::filter(!is.na(commodity)) ) %>% unlist() %>% unname()
    n_commodity<-length(data_commodities)


    palette <- custom.palette(n = n_commodity) %>% purrr::set_names(.,data_commodities)

  }



  ##Differentiate plot arguments according to number of scenario_to_analyse
  if(n.scen > 1){
    res_plot  <- ggplot2::ggplot(data=graph_data,
                                 aes(group= c(grouping),
                                     y = value, x = year, color= CAT)) +
      ggplot2::geom_point(size=0)+
      ggplot2::geom_line(aes(linetype=name))  +
      ggplot2::labs(linetype = "Scenario" ) +
      ggplot2::scale_linetype_manual(values=c("dashed","solid")) ##order automatically later

  }else{
    res_plot  <- ggplot2::ggplot(data = graph_data,
                                 aes(group= c(grouping),y = value, x = year,color= CAT)) +
      ggplot2::geom_point(size=0)+
      ggplot2::geom_line()
  }

  ##Common arguments to both types

  res_plot <-  res_plot +
    ggplot2::labs(title = title,
                  color = "" ) +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::scale_color_manual(values = palette, limits = label)

  if(template =="ofce"){
    res_plot <- res_plot + ofce::theme_ofce(base_family = "")
  }

  ##
  if(growth.rate== TRUE & abs.diff == FALSE){
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }
  if(growth.rate== TRUE & abs.diff == TRUE){
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format(suffix = ""))
  }
  if(growth.rate== FALSE & abs.diff == FALSE & diff == TRUE ){
    res_plot <- res_plot + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }

  res_plot <- res_plot +
    ggplot2::scale_x_continuous(breaks = break_x_sequence) +


    theme(axis.title.y.right = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks.y.right = element_blank(),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),

          axis.line.x.top = element_line(colour = color_outerlines, size = 0.5),
          axis.line.y.right = element_line(colour = color_outerlines, size = 0.5),
          axis.line.x.bottom = element_line(colour = color_outerlines, size = 0.5),
          axis.line.y.left = element_line(colour = color_outerlines, size = 0.5),

          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),

          panel.grid.major.y = element_line(colour = color_gridlines,size = 0.5,linetype = "dashed"),
          panel.grid.minor.y = element_line(colour = color_gridlines,size = 0.5,linetype = "dashed"),


          axis.ticks = element_line(size = 0.5, colour = "grey42")

    )

  res_plot <- res_plot + ggplot2::theme(legend.position = "bottom")

  res_plot
}



### STACKED BAR PLOTS

#' Draw stacked bar plots  for a variable decomposed into multiple sectors or commodities
#'
#' @param data a ThreeMe data.frame in long form
#' @param variable character string (1) with the root variable to plot. It must be present in the data in both aggregated form and in multiple sector/commodity form. i.e. for output, specify "Y"
#' @param group_type length 1 "sector" or "commodity" , "S" or "C" also works
#' @param scenario the scenario to plot, must be present in the data
#' @param diff If TRUE, the difference from baseline scenario will be plotted. Default is FALSE
#' @param title Title of plot to display
#' @param scenario.names Name of the scenario to display. If NULL the scenario variable will be used
#' @param startyear First year to plot. If NULL (default) reverts to earliest year available.
#' @param endyear Last year to plot. If NULL (default) reverts to lastest year available.
#' @param template Theme to use (OFCE or nothing)
#' @param interval interval of years to plot
#' @param corner_text y-axis legend to appear in top left corner
#' @param scenario.diff.ref Name of the reference scenario from which to compute the difference. Default is "baseline"
#' @param bridge4palette_sectors Sector bridge to use to get colour gradient
#' @param names4palette_sectors Sector names vs codes table
#' @param bridge4palette_commodity Commodity bridge to use to get colour gradient
#' @param names4palette_commodity Commodity names vs codes table
#'
#' @import ggplot2 dplyr tidyr ofce
#' @importFrom scales percent label_number percent_format
#' @importFrom purrr map set_names
#'
#' @return a ggplot
#' @export
stacked_sc_plot <- function(data , variable, group_type = "sector",
                            scenario = scenario_to_analyse,
                            diff = FALSE ,
                            title = "" ,
                            scenario.names = NULL,
                            interval = 10,
                            startyear = NULL, endyear = NULL,
                            scenario.diff.ref = "baseline",
                            template = template_default,
                            corner_text = "in Millions",
                            bridge4palette_sectors = bridge_sectors,
                            names4palette_sectors = names_sectors,
                            bridge4palette_commodity = bridge_commodities,
                            names4palette_commodity = names_commodities ){

  # names_sectors <- NULL
  scen.diff <- NULL
  sector <- NULL
  # . <- NULL
  commodity <- NULL
  name <- NULL
  scen.type <- NULL
  CAT <- NULL

  #########################
  ### 0. Running Checks ###
  #########################
  # browser()
  #### Checking group_type
  if(is.character(group_type)== FALSE){
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }else{
    group <- toupper(stringr::str_replace(group_type,"^(.).*$","\\1" ))
  }

  if(!group %in% c("S", "C")){
    stop(message = " Argument group_type must be a character string starting with s for sectors or c for commodities.\n")
  }

  if(group == "S"){division_type = "Sector" }
  if(group == "C"){division_type = "Commodity" }



  #### scenarios check
  if (is.null(scenario)){
    scenario <- "baseline"
  }

  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 0 ){
    cat(paste0("Scenario '",scenario,"' was not found in the database. Will plot baseline instead. \n"   ))
    scenario <- "baseline"
  }

  if (length(scenario) == 1 & prod(scenario %in% colnames(data)) == 0){
    stop(message = paste0("Scenario '",scenario,"' was not found in the database. \n"   ))
  }

  if(length(scenario) > 1 & prod(scenario %in% colnames(data)) == 0){
    scenario <- intersect(scenario , colnames(data))
    if(purrr::is_empty(scenario)){ stop(message = paste0("The specified scenarios were not found in the database. \n"   )) }
  }

  n.scen <- length(scenario)

  if(is.null(scenario.names)){scenario.names <- gsub("(_|\\.)"," ",scenario)}
  if(length(scenario.names) != n.scen){scenario.names <- gsub("(_|\\.)"," ",scenario)}



  #### diff checks
  if(diff == TRUE){
    if(is.null(scenario.diff.ref)){scenario.diff.ref <- "baseline"}
    scenario.diff.ref <- scenario.diff.ref[1]

    if(!scenario.diff.ref %in% colnames(data)){
      stop(message = paste0("The reference scenario '",scenario.diff.ref , "' was not found in the database. \n"   ) )
    }
  }

  #### Determining years to plot

  if(is.null(interval) ){
    interval = 5
  }else{
    interval <- round(interval,0)
  }

  if(is.null(startyear) ){
    startyear = min(data$year)
  }

  if(startyear < min(data$year)){
    startyear = min(data$year)
  }

  if(is.null(endyear) ){
    endyear = max(data$year)
  }

  if(endyear > max(data$year)){
    endyear = max(data$year)
  }


  years_to_plot <- unique(c(seq(from = startyear, to  = endyear, by  = interval)))

  #############################
  ### 1. Preparing the data ###
  #############################

  #### Check that the variables exists
  var_vec <- unique(data$variable)

  liste_var <- var_vec[grep(paste0("^",variable,"_",group,"[A-Z0-9]{3}$"),var_vec)]

  if(length(liste_var) == 0 ){
    liste_var
    stop(message = "No variables matching the variable and the group_type were found.\n") }

  #### Building the data_base

  ##### No diffs
  # browser()
  if(diff == FALSE){
    graph_data <- data %>%

      dplyr::filter(variable %in% liste_var) %>%
      dplyr::filter(year %in% years_to_plot) %>%
      tidyr::pivot_longer(cols = all_of(scenario))
  }else{

    ##### With diffs
    graph_data <- data %>%
      dplyr::filter(variable %in% liste_var) %>%
      dplyr::filter(year %in% years_to_plot)%>%
      dplyr::mutate(scen.diff = get(scenario.diff.ref)) %>%
      dplyr::mutate_at(.vars =  scenario,~( .x -scen.diff )) %>%
      dplyr::select(-scen.diff) %>%
      tidyr::pivot_longer(cols = all_of(scenario))


  }

  graph_data <- as.data.frame(graph_data)
  graph_data$CAT <- graph_data[,tolower(division_type)]
  graph_data$name <- stringr::str_replace_all(graph_data$name, purrr::set_names(scenario.names,scenario))


  ########################
  ### 2. Drawing Plots ###
  ########################
  color_outerlines = "grey22"

  color_gridlines = "gray84"

  #### Creating the color palette
  if(group== "S"){
    data_sectors <-unique(data %>% dplyr::select(sector) %>% dplyr::filter(!is.na(sector)) ) %>% unlist() %>% unname() %>% sort
    n_sector<-length(data_sectors)

    if(!is.null(bridge4palette_sectors) & !is.null(names4palette_sectors)){
      palette <- custom.palette(bridge_group = bridge4palette_sectors )

      names(palette)<- toupper(names(palette)) %>% str_replace_all(set_names(names4palette_sectors$name,toupper(names4palette_sectors$code)))

      palette <-palette[data_sectors]

    }else{

      palette <- custom.palette(n = n_sector) %>% purrr::set_names(data_sectors,.)

    }
  }

  if(group== "C"){
    data_commodities <-unique(data %>% dplyr::select(commodity) %>% dplyr::filter(!is.na(commodity)) ) %>% unlist() %>% unname() %>% sort
    n_commodity<-length(data_commodities)


    if(!is.null(bridge4palette_commodities) & !is.null(names4palette_commodities)){
      palette <- custom.palette(bridge_group = bridge4palette_commodities )

      names(palette)<- toupper(names(palette)) %>% str_replace_all(set_names(names4palette_commodities$name,toupper(names4palette_commodities$code)))

      palette <-palette[data_commodities]
    }else{

      palette <- custom.palette(n = n_commodity) %>% purrr::set_names(data_commodities,.)

    }
  }



  if (n.scen > 1){
    graph_data <- graph_data %>%
      dplyr::mutate(
        scen.type = as.numeric(as.factor(name))
      )

    res_plot <-  ggplot2::ggplot(data = graph_data ,aes(x=scen.type)) +
      ggplot2::geom_bar(aes(fill=factor(CAT, levels = names(palette[CAT])), y=value ),
                        position=position_stack(),
                        stat="identity" ) +

      ggplot2::scale_x_continuous(breaks = 1:n.scen,
                                  labels = scenario.names,
                                  sec.axis = dup_axis()) +

      ggplot2::facet_grid(~year, switch="x") +

      ggplot2::theme(panel.spacing.x=unit(0, "lines") ,
                     panel.spacing = unit(0, "mm"),                       # remove spacing between facets

                     strip.background = element_rect(size = 0.5,colour = "transparent"),
                     strip.placement = "outside" ,
                     strip.text.x =element_text(face = "bold")  ,

                     panel.border = element_rect(colour = color_gridlines,fill = NA,size = 0.5),
                     axis.text.x.bottom = element_text(face = 'italic'),

                     legend.position = "bottom"
      )

    if(template =="ofce"){
      res_plot <- res_plot +  ofce::theme_ofce(base_family = "")
    }

  }else{

    res_plot  <-  ggplot(data = graph_data ,aes(x=year)) +
      ggplot2::geom_bar(aes(fill=factor(CAT), y=value ),
                        position=position_stack(),
                        stat="identity" ) +

      ggplot2::scale_x_continuous(breaks = years_to_plot,    # simulate tick marks for left axis
                                  sec.axis = dup_axis()) +
      theme(panel.border = element_rect(colour = color_outerlines,fill = NA,size = 0.5),
            axis.text.x.bottom = element_text(face = 'bold'),
            legend.position = "bottom")

    if(template =="ofce"){
      res_plot <- res_plot +  ofce::theme_ofce(base_family = "")
    }

  }

  res_plot <- res_plot +
    labs(title = title,
         fill = "") +
    ggplot2::scale_fill_manual( values =  palette)+
    xlab("") +ylab("") +
    ggplot2::geom_abline(intercept = 0,slope = 0,color = color_outerlines) +
    ggplot2::scale_y_continuous(labels = function(x){paste(x)},    # simulate tick marks for left axis
                                sec.axis = dup_axis(breaks = 0)) +
    ggplot2::theme(axis.title.y.right = element_blank(),
                   axis.text.y.right = element_blank(),
                   axis.ticks.y.right = element_blank(),
                   # axis.text.y = element_text(margin = margin(l = 1)),  # move left axis labels closer to axis
                   axis.title.x.top = element_blank(),
                   axis.text.x.top = element_blank(),
                   axis.ticks.x.top = element_blank(),
                   axis.ticks.x.bottom = element_blank(),

                   axis.line.x.top = element_line(colour = "transparent", size = 0.5),
                   axis.line.y.right = element_line(colour = "transparent", size = 0.5),
                   # axis.line.x.bottom = element_line(colour = color_outerlines, size = 0.5),
                   # axis.line.y.left = element_line(colour = color_outerlines, size = 0.5),
                   #
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),

                   panel.grid.major.y = element_line(colour = color_gridlines ,size = 0.5,linetype = "dashed"),
                   panel.grid.minor.y = element_line(colour = color_gridlines, size = 0.5,linetype = "dashed")

    )
  res_plot
}
