# x_vector = sample(1:20,3,FALSE) %>% sort()
# y_vector <- sample(1:20,3,TRUE)
# side = "left"
# steps = 1
# x_vector <- c(9,10,14)
# y_vector <- c(91,19,70)
# steps = 0.5
# side = "right"
quadratric_splines <- function(x_vector, y_vector,
                               side = "left",
                               steps = 1){

  # cat("\nquadra \n")
  # cat(paste(x_vector,"\n"))

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
  coordonnees <- donnees %>% select(all_of(str_c("y",c("a","b","c"))),
                                    all_of(str_c("x",c("a","b","c")))) %>%
    unique()%>% unlist %>% as.list() %>%  imap(~assign(.y,.x,envir = local))

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


  result <- c(missing_points_y,input) %>% data.frame(x = names(.),y = .) %>% mutate(x = as.numeric(x))%>% dplyr::arrange(x)

}

#
# plop <- rbind(quadratric_splines(x_vector,y_vector,steps = 0.5, side= "left"),
#               quadratric_splines(x_vector,y_vector,steps = 0.5, side= "right")) %>%
#   unique() %>% arrange(x)
#
# plot_points(x = x_vector, y = y_vector) +
#   geom_point(data = plop %>% rename(date = x, value = y), aes(y = value),colour = "red" , size = 0.1)
