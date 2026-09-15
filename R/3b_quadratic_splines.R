#' Quadratic splines for interpolation
#'
#' @description Given 3 points A, B and C computes the interpolated values for A-B or B-C using quadratic splines
#' @param x_vector Numerical vector of length 3 of the x-axis values (ie dates) to interpolate
#' @param y_vector Numerical vector of length 3 of the y-axis values to interpolate
#' @param side Either "left" to compute A-B interpolation or "right" to compute B-C interpolation. Default: "left"
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
#' quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.5, side= "left")
#' quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.5, side= "right")
#' \dontrun{
#' ggplot(mapping = aes(x= c(9,10,14), y= c(91,19,70))) +
#' geom_point(color = "red", size = 3)+
#' geom_point(aes(x = quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.1, side= "left")$x,
#'                 y = quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.1, side= "left")$y),
#'                  color = "blue" , size= 1  )+
#'
#'  geom_point(aes(x = quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.1, side= "right")$x,
#'                 y = quadratric_splines(c(9,10,14),c(91,19,70),steps = 0.1, side= "right")$y),
#'                 color = "purple" , size= 1  ) +
#'  xlab("X")+
#'  ylab("Y")
#'  }
quadratric_splines <- function(x_vector,
                               y_vector,
                               side = "left",
                               steps = 1){

  xa <- NULL
  xb <- NULL
  ya <- NULL
  yb <- NULL
  yc <- NULL
  xc <- NULL

  # cat("\n quadra \n")
  # cat(paste(x_vector))

  if(length(x_vector) != 3 | length(x_vector) != 3){
    stop(message("x_vector and y_vector must be of length 3 each."))}

  if(side == "right"){
    x_vector<- x_vector[3:1]
    y_vector<- y_vector[3:1]
    steps = -steps
  }


  donnees <- data.frame(obs = c(0,1), ## obs servira d'indicateur de temps,
                        ya = rep(y_vector[1],2), xa = rep(x_vector[1],2),
                        yb = rep(y_vector[2],2), xb = rep(x_vector[2],2),
                        yc = rep(y_vector[3],2), xc = rep(x_vector[3],2)
  )

  local <- environment()
  coordonnees <- donnees %>% dplyr::select(dplyr::all_of(stringr::str_c("y",c("a","b","c"))),
                                           dplyr::all_of(stringr::str_c("x",c("a","b","c")))) %>%
    unique()%>% unlist %>% as.list() %>%  purrr::imap(~assign(.y,.x,envir = local))

  A <- matrix(c( xa^2 , xa , 1 ,
                 xb^2 , xb , 1 ,
                 2*xb , 1  , 0  ),
              ncol = 3 , byrow = TRUE )

  b_vec <- c(ya,
             yb,
             (yc-ya)/(xc-xa))

  x <- c("a2","a1","a0")

  solution <- solve(A,b_vec) %>% purrr::set_names(x)


  missing_points_x <- seq(from = (x_vector[1]), to = (x_vector[2]), by = steps)
  missing_points_y <- purrr::set_names(missing_points_x) %>%
    purrr::map_dbl(~solution["a2"]*.x^2 + solution["a1"]*.x + solution["a0"])

  input <- purrr::set_names(y_vector,x_vector)


  result <- c(missing_points_y,input) %>% data.frame(x = names(.),y = .) %>%
    dplyr::mutate(x = as.numeric(x))%>%
    unique() %>% dplyr::arrange(x)

  result
}

#
# plop <- rbind(quadratric_splines(x_vector,y_vector,steps = 0.5, side= "left"),
#               quadratric_splines(x_vector,y_vector,steps = 0.5, side= "right")) %>%
#   unique() %>% arrange(x)
#
# plot_points(x = x_vector, y = y_vector) +
#   geom_point(data = plop %>% rename(date = x, value = y), aes(y = value),colour = "red" , size = 0.1)



