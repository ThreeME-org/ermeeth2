## simpleplot requires selectseries to be loaded

#' Simple plot
#'
#' Créer un graphe de type geom_line à partir d'un vecteur de variables issues d'une base de données 
#' D'autres arguments de la fonction permette de choisir le type d'indicateur et de compléter les labels 
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param series character(1) string, vecteur avec le noms des variables qui seront plottées
#' @param label_series character(1) string, vecteur avec le label des variables qui seront plottées
#' @param startyear numeric(1), date de début de la période plottée
#' @param endtyear numeric(1), date de fin  de la période plottée
#' @param transformation character(1) string, choix de l'option de l'indicateur (en niveau, en variation, différence absolue, relative )
#' @param unit character(1) string , choix de legende pour l'axe des x (en pourcentage ou en niveau)
#' @param decimal numeric(1) , choix de la décimale retenue après la virgule
#' @param titleplot character(1) string , titre du graphique
#' @param scenario_name character(1) string, nom du scénario à plotter
#' #' @param template_default character(1) string, nom du thème ggplot retenu pour le plot
#' @return un ggplot
#' @import ggh4x ggplot2
#' @family function
#' @export
simpleplot <- function(data,
                       series,
                       multi_country = NULL,
                       label_series = NULL,
                       startyear = NULL,
                       endyear = NULL,
                       transformation ="reldiff",
                       unit = "percent",
                       decimal = 0.1,
                       titleplot = "",
                       scenario=scenario_name,
                       template=template_default) {
  
  # To debug the function step by step, activate line below
  # browser()
  
  
  if (is.null(multi_country)){
    selection <- selectseries(data, scenario, series, startyear, endyear, transformation)
  }else{
    data <- data %>%
            filter(variable %in% series) %>%
            mutate(variable = country)
    selection <- selectseries(data, scenario, multi_country, startyear, endyear, transformation)
    series <- multi_country
  }
  # series <- as.list(series)
  
  ## Format of the output plot
  format_img <- c("svg")  # Choose format: "png", "svg"
  
  
  
  # Palette de 4 couleurs en attendant d'en rajouter dans celles OFCE
  pal <- c("#a6cee3", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99")

  
  if (is.null(label_series)){
    label_series =  series
  }

  
  ifelse(transformation =="gr",
           plotseries <- ggplot(data = selection, aes(x=year)) + aes(group = variable) +
             geom_line(aes(y=gr_base, col=variable), linetype = "dashed") +
             geom_line(aes(y=gr_scen, col=variable), linetype = "solid"),
         
         
         
         ifelse(transformation =="level",
              plotseries <- ggplot(data = selection, aes(x=year)) + aes(group = variable) +
                    geom_line(aes(y=base, col=variable), linetype = "dashed") +
                    geom_line(aes(y=scen, col=variable), linetype = "solid"),
              
              plotseries <- ggplot(data = selection, aes(group = variable, y = transfo, x = year, color = variable))  +
                     geom_line() +
                     geom_point(size = 0)
              
         )
  )
    

  plotseries <- plotseries +
    scale_y_continuous(labels = scales::percent_format(accuracy = decimal))  +
    scale_color_manual(values = pal[1:length(series)], limits = series,
                       labels = label_series)  +
    labs(x = "", y = transformation, title = titleplot) 

  if(unit != "percent") {
    plotseries <- plotseries +  scale_y_continuous(labels = scales::label_number(accuracy = decimal, scale = unit))
  }

  ifelse(template =="ofce", 
         
    plotseries <- plotseries +  ofce::theme_ofce(base_family = "Calibri"), 
         
    plotseries <- plotseries +  theme(legend.position = "bottom") 
         ) 
      
  plotseries <- plotseries + theme(legend.title = element_blank(),
                                    axis.title.y = element_blank() ) 
    
  plotseries
}


