## contribplot requires contrib to be loaded

#' contrib.plot
#'
#' Créer un graphe de type geom_bar à partir d'un vecteur de variables issues d'une base de données
#' D'autres arguments de la fonction permette de choisir le type d'indicateur et de compléter les labels
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param series character string, vecteur avec le noms des variables qui seront plottées
#' @param label_series character string, vecteur avec le label des variables qui seront plottées
#' @param startyear numeric(1), date de début de la période plottée
#' @param endyear numeric(1), date de fin  de la période plottée
#' @param unit character(1) string , choix de legende pour l'axe des x (en pourcentage ou en niveau)
#' @param decimal numeric(1) , choix de la décimale retenue après la virgule
#' @param titleplot character(1) string , titre du graphique
#' @param template character(1) string, nom du thème ggplot retenu pour le plot
#' @param line_tot boolean. Default FALSE . TRUE will display the line for the main variable.
#' @param custom_x_breaks numeric(1), choix du break en nombres d'années
#'
#' @return  ggplot
#' @import ggh4x ggplot2 dplyr tidyr
#' @importFrom scales percent label_number
#' @export

contrib.plot <- function(data,
                         series = NULL,
                         label_series = NULL,
                         line_tot = FALSE,
                         startyear = NULL,
                         endyear = NULL,
                         custom_x_breaks = NULL,
                         unit = "percent",
                         decimal = 0.1,
                         titleplot = NULL,
                         template = template_default) {
  # To debug the function step by step, activate line below
  # browser()

  ## Format of the output plot
  format_img <- c("svg")  # Choose format: "png", "svg"

  if (is.null(series)){
    series <-  data[["variable"]] %>% unique()
  }

  if (is.null(label_series)){
    label_series <-   series
  }

  if (is.null(startyear)){startyear  =  min(data$year)}
  if (is.null(endyear)){endyear  =  max(data$year)}

  if (is.null(titleplot)){titleplot  = ""}



  ## Data filtering
  data <- data %>% dplyr::filter(year >= startyear & year <= endyear)

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



  #data$year <- lubridate::ymd(data$year, truncated = 2L)

  ## Plot making
  plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = data , aes(x = year, y = value, fill = variable),
                      stat= "identity", width = 0.9, position = position_stack(reverse = TRUE)) +
    ggplot2::scale_x_continuous(breaks = break_x_sequence) +
    ggplot2::scale_fill_manual(values = custom.palette(length(series)),
                               limits = series,
                               labels = label_series) +
    ggplot2::labs(x = "", title = titleplot)


  if(unit == "percent") {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = decimal))
  }

  if(unit != "percent") {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = decimal, scale = 1/as.numeric(unit)))
  }

  if (line_tot == TRUE){
    # Data for ploting the line
    data.2 <- data %>%
      dplyr::filter(year >= startyear & year <= endyear) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = sum(dplyr::c_across(series))) %>%
      dplyr::select(year, value)

    plot <-  plot +
      ggplot2::geom_line(data = data.2 , aes(x = year, y = value), colour = "#606060", size = .5) +
      ggplot2::geom_point(data = data.2 , aes(x = year, y = value), colour = "#202020", size = .6)
  }
  if(template =="ofce"){ plot <- plot +  ofce::theme_ofce(base_family = "") }

  plot <- plot + ggplot2::theme(legend.title = element_blank(),
                                axis.title.y = element_blank(),
                                legend.position = "bottom")

  plot
}
