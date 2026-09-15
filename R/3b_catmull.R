### Catmull-Spline Calculator
#' Catmull-Rom Cubic splines for interpolation
#'
#' @description Given 4 points A B C D, computes the interpolated values for B-C using Catmull-Rom splines
#' @param x_vector Numerical vector of length 4 of the x-axis values (ie dates) to interpolate
#' @param y_vector Numerical vector of length 4 of the y-axis values to interpolate
#' @param steps numeric (1). Interval between each x values for which to compute the interpolated values. Default: 1.
#'
#' @return named vector of values going from B to C with increments steps. x values are the names of y values of the vector.
#' @export
#'
#' @import dplyr
#' @importFrom purrr imap set_names map_dbl
#' @importFrom stringr  str_c
#' @import ggplot2
#'
#' @examples
#' catmullrom_splines(sort(sample(1:20,4,FALSE)) , sample(1:30,4,TRUE), steps = 1 )
#' \dontrun{
#'  ggplot() +
#'  geom_point(aes(x= c(7,10,17,20),
#'               y= c(91,19,70,60)),
#'           color = "red", size = 3 )+
#'  geom_point(aes(x = catmullrom_splines(c(7,10,17,20),c(91,19,70,60),steps = 0.1)$x,
#'                 y = catmullrom_splines(c(7,10,17,20),c(91,19,70,60),steps = 0.1)$y  ),
#'             color = "blue" , size= 1  ) +
#'  xlab("X")+
#'  ylab("Y")
#' }

catmullrom_splines<- function(x_vector, y_vector, steps=1){

  xb <- NULL
  xc <- NULL
  yb <- NULL
  yc <- NULL
  ya <- NULL
  xa <- NULL
  yd <- NULL
  xd <- NULL
  # . <- NULL

  # cat("\n cubic\n")
  # cat(paste(x_vector))

  # x_vector = sample(1:20,4,FALSE) %>% sort()
  # y_vector = sample(1:30,4,TRUE)

  if(length(x_vector) != 4 | length(x_vector) != 4){

  stop(message("x_vector and y_vector must be of length 4 each.\n"))}


  # normalize x_vector
  normy <- min(x_vector) -1
  x_vector <- x_vector - normy

  ###
  donnees <- data.frame(obs = c(0,1), ## obs servira d'indicateur de temps,
                        a3 = c(0,NA), a2 = c(1,NA),  a1 = c(0,NA), a0 = c(0,NA),
                        ya = rep(y_vector[1],2), xa = rep(x_vector[1],2),
                        yb = rep(y_vector[2],2), xb = rep(x_vector[2],2),
                        yc = rep(y_vector[3],2), xc = rep(x_vector[3],2),
                        yd = rep(y_vector[4],2), xd = rep(x_vector[4],2)
                        )

  local <- environment()
  coordonnees <- donnees %>% dplyr::select(dplyr::all_of(stringr::str_c("y",c("a","b","c","d"))),
                                           dplyr::all_of(stringr::str_c("x",c("a","b","c","d")))) %>%
   unique()%>% unlist %>% as.list() %>%  purrr::imap(~assign(.y,.x,envir = local))

 # ## Catmull conditions define the following system b = AX

 # yb = a3*(xb^3) + a2*(xb^2) + a1*xb + a0 #curve passes through B
 # yc = a3*(xc^3) + a2*(xc^2) + a1*xc + a0 #curve passes through C
 # (yc-ya)/(xc-xa) = 3*a3*xb^2 +  2*a2*xb + a1 #tangent in B equals slope of AC line
 # (yd-yb)/(xd-xb) = 3*a3*xc^2 +  2*a2*xc + a1 #tangent in C equals slope of BD line


 A <- matrix(c( xb^3 , xb^2 ,xb , 1,
                xc^3 , xc^2 ,xc , 1,
                3*xb^2 , 2*xb , 1 , 0,
                3*xc^2 , 2*xc , 1 , 0),
             ncol = 4 , byrow = TRUE )

  b_vec <- c(yb,
             yc,
             (yc-ya)/(xc-xa),
             (yd-yb)/(xd-xb))
  x <- c("a3","a2","a1","a0")

 solution <- solve(A,b_vec) %>% purrr::set_names(x)

 missing_points_x <- seq(from = (x_vector[2]), to = (x_vector[3]), by=steps )
 missing_points_y <- purrr::set_names(missing_points_x)  %>%
   purrr::map_dbl(~solution["a3"]*.x^3 + solution["a2"]*.x^2 + solution["a1"]*.x + solution["a0"])

 input <- purrr::set_names(y_vector,x_vector)

 result <- c(missing_points_y,input) %>% data.frame(x = names(.),y = .) %>%
   dplyr::mutate(x = as.numeric(x)+normy) %>%
   unique() %>% dplyr::arrange(x)

 result
}


# library(tidyverse)
# plot_points <- function(x=x_vector,y=y_vector){
# ggplot(data.frame(date= x, values = y), aes(x=date, y=values)) +
#     ggplot2::geom_line(colour = "grey42")+
#     ggplot2::geom_point(colour = "blue", size =0.5)+
#     ggplot2::theme_light() +
#     ggplot2::xlab("")+
#     ggplot2::ylab("")
# }






