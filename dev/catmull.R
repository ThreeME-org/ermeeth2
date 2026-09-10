## Catmull-Spline Calculator
library(tidyverse)
library(Deriv)
library(tresthor)
plot_points <- function(x=x_vector,y=y_vector){
ggplot(data.frame(date= x, values = y), aes(x=date, y=values)) +
    ggplot2::geom_line(colour = "grey42")+
    ggplot2::geom_point(colour = "blue", size =0.5)+
    ggplot2::theme_light() +
    ggplot2::xlab("")+
    ggplot2::ylab("")
}




# ## Catmull conditions define the following system
# yb = a3*(xb^3) + a2*(xb^2) + a1*xb + a0 #curve passes through B
# yc = a3*(xc^3) + a2*(xc^2) + a1*xc + a0 #curve passes through C
# (yc-ya)/(xc-xa) = 3*a3*xb^2 +  2*a2*xb + a1 #tangent in B equals slope of AC line
# (yd-yb)/(xd-xb) = 3*a3*xc^2 +  2*a2*xc + a1 #tangent in C equals slope of BD line





catmullrom_splines<- function(x_vector, y_vector, steps=1){
  # cat("\ncubic\n")
  # cat(paste(x_vector,"\n"))
  # x_vector = sample(1:20,4,FALSE) %>% sort()
  # y_vector = sample(1:30,4,TRUE)

  if(length(x_vector) != 4 | length(x_vector) != 4){

  stop(message("x_vector and y_vector must be of length 4 each."))}

  donnees <- data.frame(obs = c(0,1), ## obs servira d'indicateur de temps,
                        a3 = c(0,NA), a2 = c(1,NA),  a1 = c(0,NA), a0 = c(0,NA),
                        ya = rep(y_vector[1],2), xa = rep(x_vector[1],2),
                        yb = rep(y_vector[2],2), xb = rep(x_vector[2],2),
                        yc = rep(y_vector[3],2), xc = rep(x_vector[3],2),
                        yd = rep(y_vector[4],2), xd = rep(x_vector[4],2)
                        )

local <- environment()
 coordonnees <- donnees %>% select(all_of(str_c("y",c("a","b","c","d"))),
                                   all_of(str_c("x",c("a","b","c","d")))) %>%
   unique()%>% unlist %>% as.list() %>%  imap(~assign(.y,.x,envir = local))


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
   map_dbl(~solution["a3"]*.x^3 + solution["a2"]*.x^2 + solution["a1"]*.x + solution["a0"])

 input <- purrr::set_names(y_vector,x_vector)

 result <- c(missing_points_y,input) %>% data.frame(x = names(.),y = .) %>% mutate(x = as.numeric(x)) %>% unique()%>% dplyr::arrange(x)
}

# test <- catmullrom_splines(sample(1:20,4,FALSE) %>% sort(), sample(1:30,4,TRUE), steps = 1 )



