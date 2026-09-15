##### methods of interpolation

#' Linear interpolation
#' @description Takes a vector and computes the 2nd to the penultimate values using the first and last value of the input vector using linear interpolation, assuming each element of the vector are spaced out at regular intervals.
#'
#' @param vector a numeric vector
#'
#' @return a numeric vector of the same length as vector
#' @export
#'
#' @examples
#' linear_fill(c(1,100,2))
#' linear_fill(c(42,NA,NA,NA,32))
linear_fill <- function(vector){
  k <- length(vector)
  slope <- (vector[k]-vector[1])/(k-1)

  index_vector <- c(0:(k-1))

  res <- vector[1] + slope*(index_vector)
  res[1] <- vector[1]
  res[k] <- vector[k]
  res
}

#################
### interpolation series
#################

#' interpolation_series
#' @description Given a date vector and corresponding value vector, computes the values in between each date using some form of interpolation.
#'
#' @param date_vector numeric or Date vector to match with value vector
#' @param value_vector numeric vector of anchor values from which to compute interpolation
#' @param smoothed If FALSE, interpolation will be on done only linearly. If TRUE : If 3 observations : quadratic splines will be used. If 4 or more observations : cubic splines via Catmull-Rom method will be be used. If 2 observations only : reverts to linear interpolation. Number of observations include additional observations added to reach first.date and last.date. Default: TRUE
#' @param seq.step numeric (1). Interval between each x values for which to compute the interpolated values. Default: 1.
#' @param first.date Date from which to start the final sequence. Default: ThreeME "first_date" argument.
#' @param last.date Date where the final sequence stops. Default: ThreeME "last_date" argument.
#' @param full_results If TRUE a list containing the input, the resulting interpolation by two methods, the results as a named vector and the plot will be returned. Otherwise only  the named vector presenting the resulting values will be returned. Default: FALSE
#'
#' @param debug If TRUE, will enter debug mode. Default: FALSE.
#'
#' @return Either a named vector of values going from first.date to last.date with the chosen interpolation method (smoothed or not), or a list with multiple objects including the plot and a data.frame with linear and smoothed interpolation
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom purrr map imap reduce set_names
#' @importFrom lubridate is.Date
#' @examples
#' interpolation_series(date_vector = c(2001, 2005, 2010, 2015, 2020),
#' value_vector = c(20,30,10,25,45), first.date = 2000, last.date = 2020, full_results = TRUE)
#'
interpolation_series <- function(date_vector  = NULL,
                                 value_vector = NULL,
                                 smoothed = TRUE ,
                                 seq.step = 1 ,
                                 first.date = first_date,
                                 last.date = last_date,
                                 full_results = FALSE,
                                 debug = FALSE){

  test.r <- NULL
  test.t <- NULL
  x <- NULL
  y <- NULL
  dupes <- NULL

  if(debug){browser()}

  ### running checks
  if(is.null(date_vector)){
    stop(message="No date_vector specified.\n")
  }
  if(is.null(value_vector)){
    stop(message="No value_vector specified.\n")
  }

  if(length(date_vector) != length(value_vector)){
    stop(message="value_vector and date_vector must be of the same length.\n")
  }

  if(max(date_vector) > last.date){
    message("The specified last.date is earlier than lastest date provided in date_vector, the argument will be ignored.\n")
    last.date <- max(date_vector)
  }

  if(min(date_vector) < first.date){
    message("The specified first.date is later than earliest date provided in date_vector, the argument will be ignored.\n")
    first.date <- min(date_vector)
  }

  if(!is.numeric(value_vector)){
    stop(message = "value_vector must be numeric. \n")
  }

  input_array_def  <- input_array  <- data.frame(date = date_vector,input_vector = value_vector) %>% dplyr::arrange(date)
  index_vector_def <- index_vector <- seq(from = first.date, to = last.date, by = seq.step )

  if(prod(date_vector %in% index_vector) == 0){
    stop(message="Some dates from date_vector could not be found in the sequence from = first.date to = last.date by seq.step. Please check seq.step specification. \n")
  }

  if(lubridate::is.Date(index_vector)){
    input_array

  }

  res_array <- data.frame(date = index_vector,res_vector = rep(NA, length(index_vector))) %>%
    dplyr::full_join(input_array, by = "date") %>%
    dplyr::mutate(res_vector = ifelse(is.na(input_vector),res_vector,input_vector),
           res_vector = ifelse(date < min(date_vector),
                               input_array$input_vector[which(input_array$date == min(date_vector))] , res_vector) ,
           res_vector = ifelse(date > max(date_vector),
                               input_array$input_vector[which(input_array$date == max(date_vector))] , res_vector) ,
           test.r = dplyr::lag(date)
    )  %>% dplyr::select(-input_vector)

  ## step 1 locate the intervals that need filling in

  if(is.na(res_array$res_vector[1])){
    stop(message= "Functional error : NA found in first position")
  }
  if(is.na(res_array$res_vector[nrow(res_array)])){
    stop(message= "Functional error : NA found in last position")
  }

  non_na_dates <- data.frame(date = res_array$date[which(!is.na(res_array$res_vector))]) %>%
    dplyr::mutate(test.t = dplyr::lag(date))

  intervals <- res_array %>% dplyr::left_join(non_na_dates, by = "date") %>% dplyr::filter(test.r != test.t) %>% dplyr::select(test.t,test.r) %>% dplyr::mutate(test.r = test.r + seq.step) %>% dplyr::rename(start = test.t, end = test.r)


if(nrow(intervals)>0){na.vectors <- c(1:nrow(intervals)) %>%
    purrr::map(~c(intervals$start[.x],intervals$end[.x])) %>%
    purrr::map(~seq(from = .x[1], to = .x[2], by = seq.step)) %>%
    purrr::map(~res_array %>% dplyr::filter(date %in% .x) %>%
          dplyr::select(-test.r) %>%
          ## insert different methods here
          dplyr::mutate(res_vector = linear_fill(res_vector)) )

  final_res <-na.vectors %>%  purrr::reduce(rbind) %>%
    rbind(res_array %>% dplyr::select(date, res_vector) %>% dplyr::filter(!is.na(res_vector))) %>%
    unique() %>% dplyr::arrange(date)}else{

      final_res <- res_array %>% dplyr::select(date, res_vector) %>% dplyr::filter(!is.na(res_vector)) %>%
        dplyr::mutate_all(~round(.x,7)) %>%
        unique() %>% dplyr::arrange(date)
    }

  ###### NON LINEAR INTERPOLATION STARTS HERE
  input_array_bis <- input_array

  left_line <- FALSE
  right_line <- FALSE


  if(first.date < min(input_array$date)){
    input_array_bis <- rbind(input_array_bis , c(first(input_array$date)- seq.step , first(input_array$input_vector))) %>%
      dplyr::arrange(date)
    left_line <- TRUE
  }

  if(last.date > max(input_array$date)){
    input_array_bis <- rbind(input_array_bis , c(last(input_array$date)+ seq.step , last(input_array$input_vector))) %>%
      dplyr::arrange(date)
    right_line <- TRUE
    }

  if (nrow(input_array_bis) < 3 ){smoothed = FALSE}

  if(smoothed == TRUE){

    ###Case with 3 obs only
    if(nrow(input_array_bis) == 3){
      sides <- c("left","right")
      if(left_line == TRUE){sides <- c("right")}
      if(right_line == TRUE){sides <- c("left")}

      ## Quadratic case only
      final_res_smoothed <- sides %>%
        purrr::map(~quadratric_splines(input_array_bis$date , input_array_bis$input_vector , steps = seq.step , side= .x) ) %>%

        purrr::reduce(rbind) %>% unique() %>%
        dplyr::rename(date = x, res_vector = y) %>%
        rbind(res_array %>% dplyr::select(date, res_vector) %>% dplyr::filter(!is.na(res_vector)))%>%
        dplyr::mutate_all(~round(.x,7)) %>%
        unique() %>% dplyr::arrange(date)
    }


    if(nrow(input_array_bis) > 3){
      ##use cubic for middle and quadratic for sides
      sides <- list(left = c(1,3), right = c( (length(input_array_bis$date)-2), length(input_array_bis$date) )  )
      if(left_line == TRUE){sides$left <-NULL} ## if there is a left line, left end will not be computed quadratically
      if(right_line == TRUE){sides$right <-NULL} ## if thre is a right line, right end will  not be computed quadratically

      if(length(sides) == 0){
        res_ends_smoothed <- data.frame(date = c(first(date_vector)), res_vector = first(value_vector))
      }else{
        res_ends_smoothed <-sides %>%
          purrr::imap(~quadratric_splines(input_array_bis$date[.x[1]:.x[2]] ,
                                   input_array_bis$input_vector[.x[1]:.x[2]]  ,
                                   steps = seq.step ,
                                   side= .y) ) %>%

          purrr::reduce(rbind) %>% unique() %>%
          dplyr::rename(date = x, res_vector = y)
      }

      ## compute all cubic splines
      ## 1 determine all quartets
      input_array_bis$index_start <- 1:nrow(input_array_bis)
      input_array_bis$index_end <- dplyr::lead(input_array_bis$index_start,3)
      quartets <- input_array_bis %>%
        dplyr::filter(!is.na(index_end)) %>% dplyr::select(index_start, index_end) %>%
        t() %>% as.data.frame() %>% purrr::map(~c(.x[1]:.x[2]))

      res_smoothed_cube <- quartets %>%
        purrr::map(~catmullrom_splines(input_array_bis$date[.x],
                                       input_array_bis$input_vector[.x],
                                       steps = seq.step)) %>%

        purrr::reduce(rbind) %>% unique() %>%
        dplyr::rename(date = x, res_vector = y)

      final_res_smoothed <- rbind(res_ends_smoothed,
                                  res_smoothed_cube,
                                  (res_array %>% dplyr::select(date, res_vector) %>% dplyr::filter(!is.na(res_vector)))
                                  ) %>%
        dplyr::mutate_all(~round(.x,7)) %>%
      unique() %>% dplyr::arrange(date)






    }



  }else{final_res_smoothed <- final_res}

if(sum(duplicated(final_res_smoothed$date)) >0  ){

  final_res_smoothed <- final_res_smoothed %>% mutate(dupes = duplicated(date)) %>%
    filter(dupes == FALSE) %>% select(-dupes) %>%
    arrange(date) %>% as.data.frame()

}




compare <- left_join(final_res,final_res_smoothed %>% rename(res_vector_smoothed = res_vector), by = "date")


plot.interpol <- ggplot2::ggplot(compare, ggplot2::aes(x=date, y=res_vector)) +
      ggplot2::geom_line(colour = "grey42")+
      ggplot2::geom_point(colour = "blue", size =0.5)+
      ggplot2::geom_point(ggplot2::aes(y = res_vector_smoothed),colour = "seagreen2", size =0.5)+
      ggplot2::geom_point(data = input_array, ggplot2::aes(y = input_vector), colour = "red", size = 1.5) +
      ggplot2::theme_light() +
      ggplot2::xlab("")+
      ggplot2::ylab("Y")


result_short <- final_res_smoothed$res_vector %>% purrr::set_names(final_res_smoothed$date)


result_full <- list(input = input_array,
                    result_array = compare ,
                    plot = plot.interpol ,
                    result_vector = result_short)

if(full_results == FALSE){res <- result_short}else{res<- result_full}

res

}

# ### One Test
# random_size = sample(4:15,1)
# date_test  =  sample(1:400,random_size,FALSE) %>% sort()
# value_test = sample(1:100,random_size,TRUE)
# seq.step_test = 1
#
# first.date_test =  sample(c(1:min(date_test))  ,1)
# last.date_test  =  sample( (max(date_test)):401 ,1)
#
# interpolation_series(date_vector = date_test,
#                      value_vector = value_test,
#                      smoothed = TRUE,
#                      seq.step = seq.step_test,
#                      first.date = first.date_test,
#                      last.date = last.date_test,
#                      debug = FALSE, full_results = TRUE)

#
# ### Multiple tests
#
#
# n_test = 16
# mega_test<- c(1:n_test) %>% map(~.x)
#
# for(i in 1:n_test){
#   random_size = sample(9:20,1)
#   date_test  =  sample(1:400,random_size,FALSE) %>% sort()
#   value_test = sample(1:100,random_size,TRUE)
#   seq.step_test = sample(c(0.25,0.5,1),1)
#
#   first.date_test =  sample(c(1:min(date_test))  ,1)
#   last.date_test  =  sample( (max(date_test)):401 ,1)
#   print(i)
#
#   res<- interpolation_series(date_vector = date_test,
#                              value_vector = value_test,
#                              smoothed = TRUE,
#                              seq.step = seq.step_test,
#                              first.date = first.date_test,
#                              last.date = last.date_test
#   )
#   mega_test[[i]]<- res
# # }
#
# first_date_x <- 2000
# last_date_x <- 2050
# n = last_date_x- first_date_x +1
# #
# plop <- interpolation_series2(date_vector = c(2005,2015,2025,2035,2045,2050),
#                                       value_vector = c(500, 550, 400, 420, 400, 300),
#                                       first.date = first_date_x, last.date = last_date_x, full_results = TRUE)
# print(length(plop$result_vector) == n)
#
# test <- plop$result_array %>% filter(date == 2045)
# test[1, "res_vector"] == test[2,"res_vector"]
# test[1, "res_vector_smoothed"] - test[2,"res_vector_smoothed"]
# test[1, "date"] == test[2,"date"]
#
# first_date_x <- 2000
# last_date_x <- 2050
# n = last_date_x- first_date_x +1
# #
# plop <- interpolation_series2(date_vector = c(2000,2025,2050),
#                               value_vector = c(500, 550,300),
#                               first.date = first_date_x, last.date = last_date_x, full_results = TRUE)
# print(length(plop$result_vector) == n)
#
# test <- plop$result_array %>% filter(date == 2045)
# test[1, "res_vector"] == test[2,"res_vector"]
# test[1, "res_vector_smoothed"] - test[2,"res_vector_smoothed"]
# test[1, "date"] == test[2,"date"]
